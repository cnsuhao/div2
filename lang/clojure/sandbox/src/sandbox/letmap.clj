(ns sandbox.letmap)

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

