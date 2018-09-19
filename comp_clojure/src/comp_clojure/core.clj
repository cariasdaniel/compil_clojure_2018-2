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
	 
	 bexp = bool | <'~'> bexp | bexp boolop bool | aexp iop aexp;
	 <boolop> = '=';
	 <iop> = <'<'> | <'>'> | <'<='> | <'>='> | boolop;
	 <bool> = <'('> bexp <')'> | boolean;
	 boolean  = #'true' | #'false';
	 digits = #'[0-9]+';"
	 :output-format :hiccup))
	 
(defn parse [x] 
	(do (->> (calc x)
     (insta/transform
       {:sum +, :sub -, :mul *, :div /,
        :digits clojure.edn/read-string :exp identity})))) 
		
(defn convert-pi x 
	(do (match x
	 [:exp _] (convert-pi (second x))
	 [:sum _] 

(let [x [1 2 3]]
  (match [x]
    [[_ _ 2]] :a0
    [[1 1 3]] :a1
    [[1 2 3]] :a2
    :else :a3))