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

