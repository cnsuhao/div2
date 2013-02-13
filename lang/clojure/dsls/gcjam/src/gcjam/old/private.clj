(ns gcjam.private)

(def #^{:private true} hidden-const 3)

(defn- hidden-fn
  [n]
  (+ n 1))