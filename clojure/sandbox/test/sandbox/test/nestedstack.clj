(ns sandbox.test.nestedstack
  (:use [clojure.test]))

(defstruct hanoi :a :b :c)

(def tower
  (loop [twr (struct hanoi 1), discs [:a :b :c]]
    (let [ prv (first discs)
           cur (second discs) ]
      (if (nil? cur) twr
          (recur (assoc twr cur (+ 1 (twr prv))) (rest discs))))))

(let [{a :a, b :b, c :c} tower]
  (list a b c))

(defn kw2sym
  [k]
  )

