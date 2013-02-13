(ns sandbox.substring)

(defn substring
  ([s n] (if (< n 0) (substring s 0 n) (.substring s (min n (.length s)))))
  ([s a b]
    (let [b (if (< b 0) (+ b (.length s)) b)
          a (min (max 0 a) (.length s))
          b (min (max a b) (.length s))] 
      (.substring s a b))))

