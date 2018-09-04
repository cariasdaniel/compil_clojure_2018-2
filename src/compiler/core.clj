(defmacro defadt [adt-name & constructors] `(do
     ; Define our test function
     (defn ~(symbol (str adt-name "?")) [~'obj]
       (= ~(str adt-name) (adt-name ~'obj)))
     ; Create our type constructors
     ~@(for [[type-name & fields] constructors]
         (apply (partial emit-constructor adt-name type-name)
                 fields))))
				 
(defn- emit-constructor [adt-name type-name & fields]
  (let [type-name# (symbol type-name)
        metadata {:adt (str adt-name) :adt-type type-name#}]
    (if (empty? fields)
      ; Can't create an empty struct, so create map instead
      `(defn ~type-name# [] (with-meta {} ~metadata))

      ; Define a struct to collect arguments into
      `(defn ~type-name# [~@fields]
         (with-meta
           (struct (create-struct ~@(map keyword fields)) ~@fields)
           ~metadata)))))
		   
(defn adt-name
  [obj]
  (-> obj meta :adt))

(defn adt-type
  [obj]
  (-> obj meta :adt-type))
  
