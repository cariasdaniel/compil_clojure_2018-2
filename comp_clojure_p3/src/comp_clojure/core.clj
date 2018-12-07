(ns comp_clojure.core
  (:gen-class)
  (:require [instaparse.core :as insta]))
  
(require '[clojure.core.match :refer [match]])
  
(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(def calc
	(insta/parser
    "prog = cmd*;
	 <cmd> = loop | assign | cseq | blk | call;
	 loop = <'while'> <'('> bexp <')'> <'do'> <'{'> cmd <'}'>;
	 assign = id <':='> exp;
	 cseq = cmd <';'> cmd;
	 call = id <'('> actuals <')'>;
	 actuals = exp {<','> exp};
	 
	 blk = <'let'> <#'\\s+'> dec <'in'> <'{'> cmd <'}'>;
	 <dec> = bind | dseq;
	 bind = <'var'> id <'='> ref | <'const'> id <'='> exp | <'fn'> id abs;
	 ref = exp;
	 abs = <'('> formals <')'> <'='> blk;
	 formals = id {<','> id};
	 dseq = dec <';'> dec;
	 
	 <exp> = aexp | bexp;
	 <aexp> = term | sum | sub;
	 sum = aexp <'+'> term;
	 sub = aexp <'-'> term;
	 <term> = factor |  mul | div;
	 mul = term <'*'> factor;
	 div = term <'/'> factor;
	 <factor> = id | <'('> aexp <')'> | num;
	 
	 <bexp> = id | bool | not | eq | lt | le | gt | ge;
	 not = <'not'> bexp;
	 eq = bexp <'=='> bool | aexp <'=='> aexp;
	 lt = aexp <'<'> aexp;
	 le = aexp <'<='> aexp;
	 gt = aexp <'>'> aexp;
	 ge = aexp <'>='> aexp;
	 <bool> = <'('> bexp <')'> | boolean;
	 
	 boolean  = #'true' | #'false';
	 num = #'[0-9]+';
	 id = #'[a-zA-Z]+'"
	 :output-format :hiccup :auto-whitespace whitespace))
	 
(defn popp [x] (x 0))
				
(defn pushp [a x] (into [] (cons a x)))

(defn restp [x] (into [] (subvec x 1)))
 
(defn prninst [args] 
	(do (prn (args 0)) (prn (args 1)) (prn (args 2)) (prn (args 3)) (prn (args 4)) (prn "---------")))
		
(defn eval-pi [ctr vl env sto loc] 
	(do (prninst [ctr vl env sto loc])(match ctr
	 [:prog _] (eval-pi (ctr 1) vl env sto loc)
	 [:sum & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#SUM (subvec ctr 3)))) 
					vl env sto loc)
	 [:sub & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#SUB (subvec ctr 3)))) 
					vl env sto loc)
	 [:mul & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#MUL (subvec ctr 3)))) 
					vl env sto loc)
	 [:div & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#DIV (subvec ctr 3)))) 
					vl env sto loc)
	
	 [:eq & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#EQ (subvec ctr 3)))) 
					vl env sto loc)
	 [:lt & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#LT (subvec ctr 3)))) 
					vl env sto loc)
	 [:le & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#LE (subvec ctr 3)))) 
					vl env sto loc)
	 [:gt & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#GT (subvec ctr 3)))) 
					vl env sto loc)
	 [:ge & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#GE (subvec ctr 3)))) 
					vl env sto loc)
	 [:not & r] (eval-pi 
					(into [] 
						(concat (ctr 1) (pushp :#NOT (subvec ctr 2)))) 
					vl env sto loc)
					
	 [:num & r] (eval-pi 
					(restp (restp ctr)) 
					(pushp (clojure.edn/read-string (popp (restp ctr))) vl) 
					env sto loc)
	 [:boolean & r] (eval-pi 
					(restp (restp ctr)) 
					(pushp (clojure.edn/read-string (popp (restp ctr))) vl) 
					env sto loc)
	 [:id & r] (eval-pi 
					(restp (restp ctr)) 
					(pushp (sto (env (popp (restp ctr)))) vl) 
					env sto loc)
					
	 [:loop & r] (eval-pi
					(into [] 
						(concat (ctr 1) (pushp :#LOOP (subvec ctr 3)))) 
					(pushp (ctr 2) (pushp [(ctr 0) (ctr 1) (ctr 2)] vl)) env sto loc)
	 [:cseq & r] (eval-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (subvec ctr 3))) 
					vl env sto loc)
	 [:assign & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (pushp :#ASSIGN (subvec ctr 3)))) 
					(pushp ((ctr 1) 1) vl) env sto loc)
					
	 [:dseq & r]	(eval-pi  
						(into [] 
							(concat (ctr 1) (ctr 2) (subvec ctr 3)))
						vl env sto loc)
						
	 [:blk & r]		(eval-pi (into [] (concat (into [] (concat (ctr 1) [:#DEC])) (into [] (concat (ctr 2) (pushp :#BLK (subvec ctr 3 )))))) 
						(pushp loc vl) env sto [])
	 [:ref & r]		(eval-pi 
						(into [] 
							(concat (ctr 1) (pushp :#REF (subvec ctr 2)))) 
					vl env sto loc)
	 [:abs & r]		(eval-pi 
						(subvec ctr 3)
						(pushp [(ctr 1) (ctr 2) env] vl)  env sto loc)
						
	 [:call & r]	(eval-pi 
						(into [] (concat (subvec (ctr 2) 1) (pushp :#CALL (pushp [((ctr 1) 1) (count (subvec (ctr 2) 1))] (subvec ctr 3)))))
						vl  env sto loc)
					
	 [:bind & r]	(eval-pi
						(into [] 
							(concat (ctr 2) (pushp :#BIND (subvec ctr 3)))) 
						(pushp ((ctr 1) 1) vl) env sto loc)
						
	 [[& ex] & r]	(eval-pi
						(into [] 
							(concat (ctr 0) (subvec ctr 1))) 
						vl env sto loc)
						
;	 [:deref & r]	(eval-pi (subvec ctr 2) 
;						(pushp (env ((ctr 1) 1)) vl) env sto loc)
;	 [:valref & r]	(eval-pi (subvec ctr 2) 
;						(pushp (sto (sto (env ((ctr 1) 1)))) vl) env sto loc)
						
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	 [:#SUM & r] (eval-pi 
					(restp ctr) 
					(pushp (+ (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#SUB & r] (eval-pi 
					(restp ctr) 
					(pushp (- (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#MUL & r] (eval-pi 
					(restp ctr) 
					(pushp (* (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#DIV & r] (eval-pi 
					(restp ctr) 
					(pushp (/ (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#EQ & r] (eval-pi 
					(restp ctr) 
					(pushp (= (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#LT & r] (eval-pi 
					(restp ctr) 
					(pushp (< (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#LE & r] (eval-pi 
					(restp ctr) 
					(pushp (<= (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#GT & r] (eval-pi 
					(restp ctr) 
					(pushp (> (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#GE & r] (eval-pi 
					(restp ctr) 
					(pushp (>= (popp vl) (popp (restp vl))) (restp (restp vl))) env sto loc)
	 [:#NOT & r] (eval-pi 
					(restp ctr) 
					(pushp (not (popp vl)) (restp vl)) env sto loc)
	 [:#LOOP & r] (if (popp vl)
					(eval-pi
						(into [] 
							(concat (vl 1) (vl 2) (restp ctr))) 
						(restp (restp (restp vl))) env sto loc)
					
					(eval-pi 
						(restp ctr) 
						(restp (restp (restp vl))) env sto loc)
				 )
	 [:#ASSIGN & r]	(eval-pi 
						(restp ctr) 
						(restp (restp vl)) 
						env 
						(merge sto {(env (popp (restp vl))) (popp vl)}) loc)
						
	 [:#REF & r] 	(eval-pi 
						(restp ctr) 
						(pushp (count sto) (restp vl)) 
						env 
						(merge sto {(count sto) (popp vl)}) 
						(pushp (count sto) loc))
						
	 [:#BIND & r] 	(eval-pi 
						(restp ctr) 
						(if (or (< (count vl) 3) (not (= (type (popp (restp (restp vl)))) (type {})))) 
							(pushp {(popp (restp vl)) (popp vl)} (restp (restp vl))) 
							(pushp (merge (popp (restp (restp vl))) {(popp (restp vl)) (popp vl)}) (subvec vl 3))) 
						env sto loc)
						
	 [:#DEC & r] 	(eval-pi 
						(restp ctr) 
						(pushp env (restp vl)) 
						(merge env (popp vl)) sto loc)
						
	 [:#BLK & r] 	(eval-pi 
						(restp ctr) 
						(restp (restp vl))  
						(popp vl) 
						(apply dissoc sto loc) 
						(popp (restp vl)))
						
	 [:#CALL & r] 	(eval-pi 
						(restp ctr) 
						vl env sto loc)
						
	 :else [ctr vl env sto loc])))
	 
(defn teste [args] 
  (prn (eval-pi args [] {} {} [])))
	
;(def x (calc "let var z = 1 in{ let var true = 10 in {while (~(true==0)) do {z:=z*true; true:= true-1}}}"))
(def x (calc "let fn ass(x,y,z,v,w) = let var y = x in {y := true} in {ass(1,2,3*45,false,7)}"))

(defn -main [& args] 
  (prn (eval-pi (calc args) [] {"x" 0 "y" 1} {0 0 1 0})))
  ;let var z = 1; var y = 4 in { let var x = 3; var j = 2 in { z := j * z;y := y − x } }