(ns comp-clojure.core
  (:gen-class))
  
(ns comp-clojure.core
  (:require [instaparse.core :as insta]))
  
(require '[clojure.core.match :refer [match]])


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(def calc
	(insta/parser
    "exp = aexp | bexp;
	 <aexp> = term | sum | sub;
	 sum = aexp <'+'> term;
	 sub = aexp <'-'> term;
	 <term> = factor |  mul | div;
	 mul = term <'*'> factor;
	 div = term <'/'> factor;
	 <factor> = num | <'('> aexp <')'>;
	 
	 <bexp> = bool | not | eq | lt | le | gt | ge;
	 not = <'~'> bexp;
	 eq = bexp <'='> bool | aexp <'='> aexp;
	 lt = aexp <'<'> aexp;
	 le = aexp <'<='> aexp;
	 gt = aexp <'>'> aexp;
	 ge = aexp <'>='> aexp;
	 <bool> = <'('> bexp <')'> | boolean;
	 boolean  = #'true' | #'false';
	 num = #'[0-9]+';"
	 :output-format :hiccup))
	 
(def ctr [])
(def vl [])
		 
(defn popp [x] (x 0))
				
(defn pushp [a x] (into [] (cons a x)))

(defn restp [x] (into [] (subvec x 1)))
				  
;;(defn evaluate [x] (convert-pi ctr vl))
		
(defn convert-pi [ctr vl] 
	(do (match ctr
	 [:exp _] (convert-pi (ctr 1) vl)
	 [:sum & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#SUM (subvec ctr 3)))) 
					vl)
	 [:sub & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#SUB (subvec ctr 3)))) 
					vl)
	 [:mul & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#MUL (subvec ctr 3)))) 
					vl)
	 [:div & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#DIV (subvec ctr 3)))) 
					vl)
	
	 [:eq & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#EQ (subvec ctr 3)))) 
					vl)
	 [:lt & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#LT (subvec ctr 3)))) 
					vl)
	 [:le & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#LE (subvec ctr 3)))) 
					vl)
	 [:gt & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#GT (subvec ctr 3)))) 
					vl)
	 [:ge & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (ctr 2) (pushp :#GE (subvec ctr 3)))) 
					vl)
	 [:not & r] (convert-pi 
					(into [] 
						(concat (ctr 1) (pushp :#NOT (subvec ctr 2)))) 
					vl)
					
	 [:num & r] (convert-pi 
					(restp (restp ctr)) 
					(pushp (clojure.edn/read-string (popp (restp ctr))) vl))
	 [:boolean & r] (convert-pi 
					(restp (restp ctr)) 
					(pushp (clojure.edn/read-string (popp (restp ctr))) vl))
					
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	 [:#SUM & r] (convert-pi 
					(restp ctr) 
					(pushp (+ (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#SUB & r] (convert-pi 
					(restp ctr) 
					(pushp (- (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#MUL & r] (convert-pi 
					(restp ctr) 
					(pushp (* (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#DIV & r] (convert-pi 
					(restp ctr) 
					(pushp (/ (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#EQ & r] (convert-pi 
					(restp ctr) 
					(pushp (= (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#LT & r] (convert-pi 
					(restp ctr) 
					(pushp (< (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#LE & r] (convert-pi 
					(restp ctr) 
					(pushp (<= (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#GT & r] (convert-pi 
					(restp ctr) 
					(pushp (> (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#GE & r] (convert-pi 
					(restp ctr) 
					(pushp (>= (popp (restp vl)) (popp vl)) (restp (restp vl))))
	 [:#NOT & r] (convert-pi 
					(restp ctr) 
					(pushp (not (popp vl)) (restp vl)))
	 :else (prn ctr vl))))
	 
(defn teste [x] (convert-pi x []))
	
(def x (calc "1+2+3"))

(defn teste [] (pushp :ass x))