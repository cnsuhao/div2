(ns sas.helpers)

(defn substring
  ([s n] (if (< n 0) (substring s 0 n) (.substring s (min n (.length s)))))
  ([s a b]
    (let [b (if (< b 0) (+ b (.length s)) b)
          a (min (max 0 a) (.length s))
          b (min (max a b) (.length s))] 
      (.substring s a b))))

(defmacro let-map
  "Takes a map and binds the keywords' symbols to their values and 
   runs body.  If bingings is nil, let-map simply runs body without 
   bindings.

   Example-Area of a rectangle:
     (defn area
       [rect]
       (let-map rect
         (* width height)))
     
     (area {:width 3, height: 5})"
  [bindings & body]
  `(let [bnd# ~bindings
         vars# (mapcat (fn [k#] [(symbol (name k#)) (get bnd# k#)]) (keys bnd#)) 
         vars# (vec vars#) ]
      (eval (list 'let vars# '~@body))))

