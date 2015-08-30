(ns clojure-noob.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "I'm a little teapot!"))

(defn train
  []
  (println "Choo chooo"))
(+ 1 (* 2 3) 4)

1

"A string"

["a" "vector" "of" "strings"]


(if true
  (do (println "Success!")
      "By Zeus's hammer!")
  (do (println "Failure!")))

;; nils
(nil? 1)
;=> false


;; naming things with def
(def failed-protagonist-names
  ["Larry Potter" "Doreen the Explorer" "The Incredible Bulk"])


;; numbers
94
1.2
1/6

;; keywords
:a
:rumplestiltsken
:34
:_?

(vector "creepy" "full" "moon")


(contains? #{:a :b} :a
           )
; => true
(contains? #{:a :b} 3)
; => false
(contains? #{nil} nil)
                                        ; => true

(inc 1.1)
                                        ;=> 2.1
(+ (inc 199) (/ 100 (- 7 2)))
(+ 200 (/ 100 (- 7 2))) ; evaluated "(inc 199)"
(+ 200 (/ 100 5)) ; evaluated (- 7 2)
(+ 200 20) ; evaluated (/ 100 5)
220 ; final evaluation

;; To get doc fn in repl:
;; (use 'clojure.repl)
;; (doc -main) ;;=> now works!

;; anonymous functions
(#(* % 3) 8)

;; The Shire’s Next Top Model
(def asym-hobbit-body-parts [{:name "head" :size 3}
                                          {:name "left-eye" :size 1}
                                          {:name "left-ear" :size 1}
                                          {:name "mouth" :size 1}
                                          {:name "nose" :size 1}
                                          {:name "neck" :size 2}
                                          {:name "left-shoulder" :size 3}
                                          {:name "left-upper-arm" :size 3}
                                          {:name "chest" :size 10}
                                          {:name "back" :size 10}
                                          {:name "left-forearm" :size 3}
                                          {:name "abdomen" :size 6}
                                          {:name "left-kidney" :size 1}
                                          {:name "left-hand" :size 2}
                                          {:name "left-knee" :size 2}
                                          {:name "left-thigh" :size 4}
                                          {:name "left-lower-leg" :size 3}
                                          {:name "left-achilles" :size 1}
                                          {:name "left-foot" :size 2}])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts
                     (set [part (matching-part part)])))))))

(symmetrize-body-parts asym-hobbit-body-parts)

;; loop
(loop [iteration 0]
  (println (str "Iteration " iteration))
  (if (> iteration 3)
    (println "Goodbye!")
    (recur (inc iteration))))

;; could do the same thing with functions:

(defn recursive-printer
  ([]
     (recursive-printer 0))
  ([iteration]
     (println iteration)
     (if (> iteration 3)
       (println "Goodbye!")
       (recursive-printer (inc iteration)))))
(recursive-printer)

;;
(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set [part (matching-part part)])))
          []
          asym-body-parts))

(better-symmetrize-body-parts asym-hobbit-body-parts)

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + (map :size sym-parts))
        target (rand body-part-size-sum)]
    (loop [[part & remaining] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur remaining (+ accumulated-size (:size (first remaining))))))))
