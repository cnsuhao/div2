(ns gcjam.helpers)

;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;
(defn substring
  ([s n] (if (< n 0) (substring s 0 n) (.substring s (min n (.length s)))))
  ([s a b]
    (let [b (if (< b 0) (+ b (.length s)) b)
          a (min (max 0 a) (.length s))
          b (min (max a b) (.length s))] 
      (.substring s a b))))

(defmacro letmap
  [obj & body]
  (if (nil? obj) 
    `(do ~@body)
    `(let [ ~@(mapcat (fn [k] [(. k sym) (k obj)]) (keys obj)) ]
       ~@body)))

(defn by-pairs
  [coll]
  (letfn [ (take-pair [c] (when (next c) (take 2 c))) ]
    (lazy-seq
      (when-let [pair (seq (take-pair coll))]
        (cons pair (by-pairs (rest coll)))))))

(defn atoi
  [a]
  (Integer/parseInt a))

(defn datetime-time
  [[_ h m pm]]
  (let [h (atoi h)
        m (atoi m)
        pm (if (=  "p" (.toLowerCase pm)) 12 0)]
   [ (+ h pm) m ]))

(def not-empty? (comp not empty?))
(def self (fn [x & _] x))

(def eval-template (comp eval macroexpand))

;;;;;;;;;;;;;
;; GENERAL ;;
;;;;;;;;;;;;;
(defn valid-pair
  [pair]
  (not (string? (first pair))))

(defn correct-pair
  [pair]
  (if (= (last pair) "") 
    (list (first pair) nil)
    pair))