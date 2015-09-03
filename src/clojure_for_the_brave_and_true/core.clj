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
