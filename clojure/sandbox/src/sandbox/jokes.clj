(ns sandbox.jokes)

;; function answers
;; What happens when you cross and elephant with a rhino?
(def len #(. % length))

(defmulti cross
   (fn [a b] (cond (= (len a) (len b)) :inter
                   (< (len a) (len b)) :swap )))

(defmethod cross :swap [a b] (cross b a))
(defmethod cross :inter [a b] (apply str (interleave a b)))
(defmethod cross :default [a b]
  (let [ diff (- (len a) (len b)) 
         s1 (take (- (len a) diff) a)
         s2 (drop (- (len b) diff) b) ]
    (apply str (concat s1 s2))))
