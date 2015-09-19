(ns clojure-for-the-brave-and-true.async)

(defmacro wait
  "Sleep `timeout` seconds before evaluating body"
  [timeout & body]
  `(do (Thread/sleep ~timeout) ~@body))

;; What the expansion of the enqueue macro will look like:
(comment
  (let [saying3 (promise)]
    (future (deliver saying3 (wait 100 "Cheerio!")))
    @(let [saying2 (promise)]
       (future (deliver saying2 (wait 400 "Pip pip!")))
       @(let [saying1 (promise)]
          (future (deliver saying1 (wait 200 "'Ello, gov'na!")))
          (println @saying1)
          saying1)
       (println @saying2)
       saying2)
    (println @saying3)
    saying3))

;; Ideally, the macro will work like:
(comment
  (-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
      (enqueue saying (wait 400 "Pip pip!") (println @saying))
      (enqueue saying (wait 100 "Cheerio!") (println @saying))))

(defmacro enqueue
  ([q concurrent-promise-name concurrent serialized]
   `(let [~concurrent-promise-name (promise)]
      (future (deliver ~concurrent-promise-name ~concurrent))
      (deref ~q)
      ~serialized
      ~concurrent-promise-name))
  ([concurrent-promise-name concurrent serialized]
   `(enqueue (future) ~concurrent-promise-name ~concurrent ~serialized)))

  (time @(-> (enqueue saying (wait 200 "'Ello, gov'na!") (println @saying))
                        (enqueue saying (wait 400 "Pip pip!") (println @saying))
                        (enqueue saying (wait 100 "Cheerio!") (println @saying))))
