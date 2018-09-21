(ns comp-clojure.core
  (:gen-class))
  
(ns comp-clojure.core
  (:require [instaparse.core :as insta]))
  
(require '[clojure.core.match :refer [match]])


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
  
(def as-and-bs
	(insta/parser
    "S = AB*;
     AB = A | B;
     A = 'a'+;
     B = 'b'+;"))

(def calc
	(insta/parser
    "exp = aexp | bexp;
	 <aexp> = term | sum | sub;
	 sum = aexp <'+'> term;
	 sub = aexp <'-'> term;
	 <term> = factor |  mul | div;
	 mul = term <'*'> factor;
	 div = term <'/'> factor;
	 <factor> = digits | <'('> aexp <')'>;
	 
	 <bexp> = bool | not | eq | ls | le | gr | ge;
	 not = <'~'> bexp;
	 eq = bexp <'='> bool;
	 ls = aexp <'<'> aexp;
	 le = aexp <'<='> aexp;
	 gr = aexp <'>'> aexp;
	 ge = aexp <'>='> aexp;
	 <bool> = <'('> bexp <')'> | boolean;
	 boolean  = #'true' | #'false';
	 digits = #'[0-9]+';"
	 :output-format :hiccup))
		
(defn convert-pi [x] 
	(match x
	 [:exp _] (convert-pi (second x))
	 [:sum _ _] (+ (convert-pi (get x 1) (get x 2)))
	 [:num _] (get x 1)
	 :else x))
	 
(defn teste [x] (convert-pi x [] []))
	
(let [x [1 2 3]]
  (match [x]
    [[_ _ 2]] :a0
    [[1 1 3]] :a1
    [[1 2 3]] :a2
    :else :a3))