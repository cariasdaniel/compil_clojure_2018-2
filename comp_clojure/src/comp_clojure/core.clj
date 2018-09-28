(ns comp-clojure.core
  (:gen-class))
  
(ns comp-clojure.core
  (:require [instaparse.core :as insta]))
  
(require '[clojure.core.match :refer [match]])


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
  
(def whitespace
  (insta/parser
    "whitespace = #'\\s+'"))

(def calc
	(insta/parser
    "prog = cmd*;
	 <cmd> = loop | assign | cseq;
	 loop = <'while'> <'('> bexp <')'> <'do'> <'{'> cmd <'}'>;
	 assign = id <':='> exp;
	 cseq = cmd <';'> cmd;
	 
	 <exp> = aexp | bexp;
	 <aexp> = term | sum | sub;
	 sum = aexp <'+'> term;
	 sub = aexp <'-'> term;
	 <term> = factor |  mul | div;
	 mul = term <'*'> factor;
	 div = term <'/'> factor;
	 <factor> = id | <'('> aexp <')'> | num;
	 
	 <bexp> = id | bool | not | eq | lt | le | gt | ge;
	 not = <'~'> bexp;
	 eq = bexp <'='> bool | aexp <'='> aexp;
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
		
(defn eval-pi [ctr vl env mem] 
	(do (match ctr
	 [:prog _] (eval-pi (ctr 1) vl env mem)
	 [:sum & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#SUM (subvec ctr 3)))) 
					vl env mem)
	 [:sub & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#SUB (subvec ctr 3)))) 
					vl env mem)
	 [:mul & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#MUL (subvec ctr 3)))) 
					vl env mem)
	 [:div & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#DIV (subvec ctr 3)))) 
					vl env mem)
	
	 [:eq & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#EQ (subvec ctr 3)))) 
					vl env mem)
	 [:lt & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#LT (subvec ctr 3)))) 
					vl env mem)
	 [:le & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#LE (subvec ctr 3)))) 
					vl env mem)
	 [:gt & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#GT (subvec ctr 3)))) 
					vl env mem)
	 [:ge & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (ctr 1) (pushp :#GE (subvec ctr 3)))) 
					vl env mem)
	 [:not & r] (eval-pi 
					(into [] 
						(concat (ctr 1) (pushp :#NOT (subvec ctr 2)))) 
					vl env mem)
					
	 [:num & r] (eval-pi 
					(restp (restp ctr)) 
					(pushp (clojure.edn/read-string (popp (restp ctr))) vl) 
					env mem)
	 [:boolean & r] (eval-pi 
					(restp (restp ctr)) 
					(pushp (clojure.edn/read-string (popp (restp ctr))) vl) 
					env mem)
	 [:id & r] (eval-pi 
					(restp (restp ctr)) 
					(pushp (mem (env (popp (restp ctr)))) vl) 
					env mem)
					
	 [:loop & r] (eval-pi
					(into [] 
						(concat (ctr 1) (pushp :#LOOP (subvec ctr 3)))) 
					(pushp (ctr 2) (pushp [(ctr 0) (ctr 1) (ctr 2)] vl)) env mem)
	 [:cseq & r] (eval-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (subvec ctr 3))) 
					vl env mem)
	 [:assign & r] (eval-pi 
					(into [] 
						(concat (ctr 2) (pushp :#ASSIGN (subvec ctr 3)))) 
					(pushp ((ctr 1) 1) vl) env mem)
					
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
	
	 [:#SUM & r] (eval-pi 
					(restp ctr) 
					(pushp (+ (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#SUB & r] (eval-pi 
					(restp ctr) 
					(pushp (- (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#MUL & r] (eval-pi 
					(restp ctr) 
					(pushp (* (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#DIV & r] (eval-pi 
					(restp ctr) 
					(pushp (/ (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#EQ & r] (eval-pi 
					(restp ctr) 
					(pushp (= (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#LT & r] (eval-pi 
					(restp ctr) 
					(pushp (< (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#LE & r] (eval-pi 
					(restp ctr) 
					(pushp (<= (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#GT & r] (eval-pi 
					(restp ctr) 
					(pushp (> (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#GE & r] (eval-pi 
					(restp ctr) 
					(pushp (>= (popp vl) (popp (restp vl))) (restp (restp vl))) env mem)
	 [:#NOT & r] (eval-pi 
					(restp ctr) 
					(pushp (not (popp vl)) (restp vl)) env mem)
	 [:#LOOP & r] (if (popp vl)
					(eval-pi
						(into [] 
							(concat (vl 1) (vl 2) (restp ctr))) 
						(restp (restp (restp vl))) env mem)
					
					(eval-pi 
						(restp ctr) 
						(restp (restp (restp vl))) env mem)
				 )
	 [:#ASSIGN & r] (eval-pi 
						(restp ctr) 
						(restp (restp vl)) 
						env 
						(merge mem {(env (popp (restp vl))) (popp vl)}))
	 :else [ctr vl env mem])))
	 
(defn teste [x] (eval-pi x []))
	
(def x (calc "while(~(x<5))do{x:=x+1;y:=x};y:=x=y"))

