(ns logical-interpreter)
(defn isFacts [x]
    (re-find #"^[^\(]*\([^)]*\)$" x)
)
(defn isRule [x]
    (re-find #"^[^\(]*\([^)]*\):-([^\(]*\([^)]*\), *)*([^\(]*\([^)]*\))$" x)
)
(defn searchElem [x lista]
  (if (empty? lista) false
      (if (= x (first lista)) true (searchElem x (rest lista)))
  )
)
(defn evaluateFacts [x lista]
 (if (searchElem x lista) (println "SI") (println "NO"))
)
(defn evaluateRule [x]
)
(defn generarLista [x lista]
    (cons x lista)
)
(defn evaluate-query [database query]
(def listaFacts '())
(def listaRule '())
     (def database2 (clojure.string/split database #"\.+"))
     (doseq [x database2]
      (def elemento1 (clojure.string/replace x #"(\t|\n)" ""))
      (def elemento2 (clojure.string/replace elemento1 #"(\t|\s)" ""))
      (if (isFacts elemento2) (def listaFacts (generarLista elemento2 listaFacts)))
      (if (isRule elemento2)  (def listaRule (generarLista elemento2 listaRule)))
     )
   
      (def query1 (clojure.string/replace query #"(\t|\n)" ""))
      (def query2 (clojure.string/replace query1 #"(\t|\s)" "")) 
      (if (isFacts query2) (evaluateFacts query2 listaFacts)) 
     ; (if (isRule query2) (evaluateRule query2 listaRule)) 


)
