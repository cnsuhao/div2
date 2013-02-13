(ns sandbox.math)

;; simple macro to make math more "traditional"
;; not very robust
(defmacro math
  [i & rst]
  (let [ops (partition 2 rst)]
    `(-> ~i
      ~@ops)))
