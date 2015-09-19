(ns clojure-for-the-brave-and-true.core
  (:gen-class))

;; Chapter 3 Exercises
;; 1. Use the str, vector, list, hash-map, and hash-set functions.
(defn built-in-functions
  []
  (vec '((str "foo" " " 123)
         (list 1 2 3 4)
         (hash-map :a "b" :c "d")
         (hash-set :a "b" :a "d"))))
;; 2. Write a function that takes a number and adds 100 to it.
(defn add-one-hundred
  [num]
  (+ num 100))

(defn -main
  "runs exercise code"
  [& args]
  (println "Hello, World!")
  ;; Chapter 3 exercises
  (println (built-in-functions))
  (println (add-one-hundred 2)))

;; 3. Write a function, dec-maker, that works exactly like the function inc-maker except with subtraction:
(defn dec-maker [num]
  #(- % num))
(def dec9 (dec-maker 9))
(dec9 10)

;; 4. Write a function, mapset, that works like map except the return value is a set:
(defn mapset [f coll]
  (set (map f coll)))

;; Chapter 4 notes

(into {} (seq {:a 1 :b 2 :c 3}))
                                        ;=> {:a 1 :b 2 :c 3}


;; about `into`:
;; If into were asked to describe its strengths at a job interview, it would say, “I’m great at taking two collections and adding all the elements from the second to the first.”

;; Function Functions

;;; apply

(max 0 1 2)
(apply max [0 1 2])

(defn my-into
  [target additions]
  (apply conj target additions))

;;; partial

(def add20 (partial + 20))

(def not-nil? (complement nil?))

;; Chapter 5
((comp inc *) 3 4)
                                        ;=> 13

(def character
  {:name "Smooches McCute"
   :attributes {:intelligence 10
                :strength 4
                :dexterity 5}})

(def c-int (comp :intelligence :attributes))
(def c-str (comp :strength :attributes))
(def c-dex (comp :dexterity :attributes))

; (fn [c] (:strength (:attributes c)))

(defn spell-slots
  [char]
  (int (inc (/ (c-int char) 2))))
(spell-slots character)

(def spell-slots-comp (comp int inc #(/ % 2) c-int))

;;;; Chapter 9 Exercises
;; 1. Write a function that takes a string as an argument and searches for it on Bing and Google using the slurp function. Your function should return the HTML of the first page returned by the search.
(defn goog-search-url
  [search-term]
  (str "https://www.google.com/search?hl=en&q=" search-term))

(defn bing-search-url
  [search-term]
  (str "https://www.bing.com/search?q=" search-term))

(defn search [search-term]
  (let [result (promise)]
    (doseq [engine [goog-search-url bing-search-url]]
      (future (if-let [results-page (slurp (engine search-term))]
                (deliver result results-page))))
    @result))

;; 2. Update your function so it takes a second argument consisting of the search engines to use.

(defn search-on-engines [search-term & engine-names]
  (let [engines {:bing bing-search-url :google goog-search-url}
        result (promise)]
    (doseq [engine-name engine-names]
      (future (if-let [results-page (slurp ((get engines engine-name) search-term))]
                (deliver result results-page))))
    (deref result 5000 :timeout)))

;; Note that Google doesn't seem to work, it always returns a 403

;; 3. Create a new function that takes a search term and search engines as arguments, and returns a vector of the URLs from the first page of search results from each search engine.

(defn results-from-search [search-term & engines]
  (let [results-page (search-on-engines search-term engines)
        matches (re-seq #"href=\"([^\" ]*)\"" results)
        uris (map second matches)
        external-links (filter #(re-find #"http(?s)://" %) uris)]
    external-links))
