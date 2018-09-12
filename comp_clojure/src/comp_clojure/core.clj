(ns comp-clojure.core
  (:gen-class))
  
(ns comp-clojure.core
  (:require [instaparse.core :as insta]))

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
    "exp = aexp;
	 <aexp> = term | sum | sub;
	 sum = aexp <'+'> term;
	 sub = aexp <'-'> term;
	 <term> = factor | mul | div;
	 mul = term <'*'> factor;
	 div = term <'/'> factor;
	 <factor> = digits | <'('> aexp <')'>;
	 digits = #'[0-9]+';"))
	 
	 (->> (calc "5+(3*8)")
     (insta/transform
       {:sum +, :sub -, :mul *, :div /,
        :digits clojure.edn/read-string :exp identity}))