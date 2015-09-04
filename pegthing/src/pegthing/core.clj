(ns pegthing.core
  (:require [clojure.set :as set])
  (:gen-class))

(declare successful-move prompt-move game-over query-rows)

;; in the data structure, each position is associated with a map like
;; {:pegged true, :connections {6 3, 4 2}}

(defn tri*
  "Generates lazy sequence of triangular numbers"
  ([] (tri* 0 1))
  ([sum n]
     (let [new-sum (+ sum n)]
       (cons new-sum (lazy-seq (tri* new-sum (inc n)))))))

(defn triangular?
  "Is the number triangular? e.g. 1, 3, 6, 10, 15, etc"
  [n]
  (= n (last (take-while #(>= n %) tri))))
;;(triangular? 5)
;;=> false
;;(triangular? 6)
;;=> true

(defn row-tri
  "The triangular number at the end of row n"
  [n]
  (last (take n tri)))

;; (row-tri 1)
;;=> 1
;; (row-tri 10)
;;=> 55
;; (row-tri 3)
;;=> 6

(defn row-num
  "Returns row number the position belongs to: pos 1 in row 1,
  positions 2 and 3 in row 2, etc"
  [pos]
  (inc (count (take-while #(> pos %) tri))))

;;(row-num 1)
;;=> 1
;;(row-num 5)
;;=> 3

(defn connect
  "Form a mutual connection between two positions"
  [board max-pos pos neighbor destination]
  (if (<= destination max-pos)
    (reduce (fn [new-board [p1 p2]]
              (assoc-in new-board [p1 :connections p2] neighbor))
            board
            [[pos destination] [destination pos]])
    board))
