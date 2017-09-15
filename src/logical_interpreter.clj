(ns logical-interpreter)
(defn isFacts [x]
  (re-find #"^[^\(]*\([^)]*\)$" x)
)
(defn isRule [x]
  (re-find #"^[^\(]*\([^)]*\):-([^\(]*\([^)]*\), *)*([^\(]*\([^)]*\))$" x)
)
(defn evaluateFacts [elem listFacts]
  ;Checks if item ej:"varon(juan)" is in the list of Facts, ex: (padre(roberto,cecilia) varon(juan) mujer(maria))
  (if (empty? listFacts) false
      (if (= elem (first listFacts)) true (evaluateFacts elem (rest listFacts)))
  )
)
(defn generateMap [listVariables listValues mapVariables]
  ;Generate hash-map relating variables with value, ex: (X Y) and (pepe juan) -> {X pepe, Y juan}
  (cond 
    (and (empty? listVariables) (empty? listValues)) mapVariables
    (empty? listVariables) (println "NO")
    (empty? listValues) (println "NO")
    :else (generateMap (rest listVariables) (rest listValues) (assoc mapVariables  (first listVariables) (first listValues)))
  )
)
(defn assignValue [text mapVariables listFacts]
  (def text2 text)
  ;Replace the variable by the value, ex: varon(X),padre(Y,X) -> varon(pepe),padre(juan,pepe)
  (doseq [key (keys mapVariables)]
	   (def text2 (clojure.string/replace text2 key (get mapVariables key)))
	)
  ;Generate vector, ej: varon(pepe),padre(juan,pepe) -> [varon(pepe) padre(juan,pepe)]
  (def listFactsQuery (clojure.string/split (clojure.string/replace text2 #"\)," ") ") #" "))
  ;Check if the list of Query Facts are in the Facts list of the database, if one fails is false.
  (def valid true)
  (doseq [val listFactsQuery :while (= true valid)]
     (def valid (evaluateFacts val listFacts)) 
  )
  (if valid (println "SI") (println "NO"))
)
(defn evaluateRule [query listRules listFacts]
  ;Get the Query name, ex: varon(maria) -> varon
  (def xlist (clojure.string/split (clojure.string/replace query #"(\)|\()" " ") #" "))
  (def nameQuery (first xlist))
  ;Search the list of Rules by Query Name, ex: hijo -> (hija(X,Y):-mujer(X),padre(Y,X) hijo(X,Y):-varon(X),padre(Y,X))
  (def valid false)
  (doseq [y listRules :while (not= true valid)]
      (def yElem y)
      (def newFacts (clojure.string/split (clojure.string/replace y #"(\)|\()" " ") #" "))
      (def nameRule (first newFacts))
      (if (= nameRule nameQuery) (def valid true) (def valid false)) 
  )
  ;ex: hijo(pepe,juan) ->[pepe juan]
  (def valueList (clojure.string/split (first (rest xlist)) #",")) 
  ;ex: hija(X,Y) -> [X Y]
  (def varList (clojure.string/split (first (rest newFacts)) #","))

  (if (= valid true) 
    ;In assignValue enter as parameters, ex: 1-> varon(X),padre(Y,X), 2-> {X pepe, Y juan}, 3-> lista de Facts
    (assignValue (first (rest (clojure.string/split yElem #":-"))) (generateMap varList valueList nil) listFacts) (println "NO")
  )
)
(defn evaluate-query [database query]
  ;Load all the Facts in a list, in other list all the Rules that exist in database
  (def listFacts '())
  (def listRule '())
  (doseq [x (clojure.string/split database #"\.+")]
      (def elem (clojure.string/replace x #"(\t|\n|\s)" ""))
      (if (isFacts elem) (def listFacts (cons elem listFacts)))
      (if (isRule elem)  (def listRule (cons elem listRule)))
  )
  ;check if the query is facts, else, check if the query is Rule
  (def q (clojure.string/replace query #"(\t|\n|\s)" ""))
  (if (evaluateFacts q listFacts) (println "SI") (evaluateRule q listRule listFacts))
)