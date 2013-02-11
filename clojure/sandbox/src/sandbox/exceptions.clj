(ns sandbox.exceptions)
"Really pointless in hindsight, should just use clojure.contrib.condition"

(defmacro deferror
  [nm base & [args & body :as cnst]]
  (let [msg `msg# cause `cause# ]
  `(defn ~nm
     ~(if (nil? cnst) ['& [msg cause]] args)
     (let 
       ~(if (nil? cnst) []
          ['v (cons 'do body)
           [msg cause] '(if (string? v) [v nil] v) ])
       (proxy [~base] [~msg ~cause])))))


(deferror MyError RuntimeException
  [pth] (format "%s was not found" pth))
(MyError "test")