(ns fwpd.core)

(def filename "suspects.csv")
;;(slurp filename)

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

;;(convert :glitter-index "3")

(defn parse
  "Convert a CSV file into rows and columns"
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

;;(parse (slurp filename))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

;; (mapify (parse (slurp filename)))

;; (first (mapify (parse (slurp filename))))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

;; (glitter-filter 3 (mapify (parse (slurp filename))))

;;;; Exercises
;; 1. Turn the result of your glitter filter into a list of names.

(defn suspect-list
  [filtered-suspects]
  (map #(% :name) filtered-suspects))

;; (suspect-list (glitter-filter 3 (mapify (parse (slurp filename)))))
;;=> ("Edward Cullen" "Jacob Black" "Carlisle Cullen")

;; 2. Write a function, append, which will append a new suspect to your list of suspects.

(defn append
  [lst & ]
  (apply conj suspects lst))

;; (append (glitter-filter 3 (mapify (parse (slurp filename)))) {:name "Matt" :glitter-index 3})
;;=> ({:name "Carlisle Cullen", :glitter-index 6} {:name "Jacob Black", :glitter-index 3} {:name "Edward Cullen", :glitter-index 10} {:name "Matt", :glitter-index 3})

;; 3. Write a function, validate, which will check that :name and :glitter-index are present when you append. The validate function should accept two arguments: a map of keywords to validating functions, similar to conversions, and the record to be validated.

(defn validate
  [validations-map record]
  ;;(reduce )
  )

