;  quadtree.clj
;
;  A baisc implementation of a point quadtree.
;
;  Trees assume origin to be in the top-left

(ns sas.quadtree)

;;;;;;;;;;;;;;;
;;  Helpers  ;;
;;;;;;;;;;;;;;;

(defn out-of-bounds
  "Checks to see if the point in question is even between the bounds of the given node"
  ([tree pt] (out-of-bounds tree (:x pt) (:y pt)))
  ([{x :x, y :y, w :w, h :h} px py]
    (or
      (< px x)
      (> px (+ x w))
      (< py y)
      (> py (+ y h)))))

(defn which-quadrent
  "Figures out which quadrent a point belongs in based on it's relative position to the middle of the provided node"
  ([tree {x :x, y :y :as pt}] (which-quadrent tree x y))
  ([tree x y]
  (let [mx (+(:x tree) (/ (:w tree) 2))
        my (+(:y tree) (/ (:h tree) 2))]
    (if (< x mx)
      (if (< y my) :nw :sw)
      (if (< y my) :ne :se)))))

(defn get-quadrent
  "Returns the quadrent provided by which-quadrent"
  [tree & args]
  (get tree (apply which-quadrent tree args)))
    
(defn get-depth
  "Calculates how many layers of branches there are"
  ([tree] (get-depth tree 1))
  ([tree depth]
    (if (not (isa? ::branches (:data tree)))
      depth
      (max 
        (get-depth (:nw tree) (inc depth))
        (get-depth (:sw tree) (inc depth))
        (get-depth (:ne tree) (inc depth))
        (get-depth (:se tree) (inc depth))))))

(defn get-leaf-count
  "Calculates now many nodes actually have points stored in them"
  [tree]
  (if (not (isa? ::branches (:data tree)))
    (if (nil? (:data tree)) 0 1)
    (+
       (get-leaf-count (:nw tree))
       (get-leaf-count (:sw tree))
       (get-leaf-count (:ne tree))
       (get-leaf-count (:se tree)))))

(defn get-branches
  "Returns all branch nodes"
  [{nw :nw, sw :sw, ne :ne, se :se :as tree}]
  [ne, se, sw, nw])

(defn apply-to-branches
  "Applies a given function to all branch nodes.  The branch is the first argument"
  [{nw :nw, sw :sw, ne :ne, se :se :as tree} f & args]
  (map (fn [branch] (apply f branch args)) [ne, se, sw, nw]))

(defn apply-to-branches*
  "Applies a given function to all branch nodes.  The branch is the last argument"
  [{nw :nw, sw :sw, ne :ne, se :se :as tree} f & args]
  (let [func (if (not-empty args) (apply partial f args) f)]
    (map func [ne, se, sw, nw])))
  

;;;;;;;;;;;;;;;
;;  Records  ;;
;;;;;;;;;;;;;;;

;; a simple pointer record
(defrecord QuadTreePoint [x y])

(defprotocol QuadTreeProtocol
  (insert [this pt] [this x y])
  (delete [this pt] [this x y])
  (query [this pt] [this x y])
  (split [this])
  (merge-split [this]) 
  (delete-all [this])
  (list-all [this]))

(defrecord QuadTreeNode [x y w h data ne se nw sw]
  Object
  (toString [this]
    (let [{x :x, y :y, w :w, h :h} this
          depth (get-depth this)
          ct (get-leaf-count this)]
      (format "Quadtree: (%s, %s) %sx%s %s branches, %s leaves" x y w h depth ct)))
  
  QuadTreeProtocol
  (insert [this x y] (insert this (QuadTreePoint. x y)))
  (delete [this x y] (delete this (QuadTreePoint. x y)))
  (query [this x y] (query this (QuadTreePoint. x y)))
  
  ;; Primary insert method, adds more points to the quadtree
  (insert [this {x :x, y :y, :as pt}] 
    (when (out-of-bounds this pt)
      (throw (IllegalArgumentException. "Point is out of the QuadTree's bounds")))
    
    (cond
      ; if empty, just add it
      (nil? (:data this)) 
        (assoc this :data pt)
      
      ; if it is the same point, return
      (= pt (:data this)) this
      
      ; if already split, insert it into the correct quadrent
      (isa? (:data this) ::branches)
        (let [qd (which-quadrent this pt)]
          (assoc this qd (insert (get this qd) pt)))
      
      ; must split, insert point into split branch
      :else (-> this (split) (insert pt))))
  
  (split [this]
    (let [x (:x this) 
          y (:y this)
          hw (/ (:w this) 2)
          hh (/ (:h this) 2)
          pt (:data this)] 
      (-> this
        (assoc :data ::branches)
        (assoc :nw (QuadTreeNode. x y hw hh nil nil nil nil nil))
        (assoc :ne (QuadTreeNode. (+ x hw) y hw hh nil nil nil nil nil))
        (assoc :sw (QuadTreeNode. x (+ y hh) hw hh nil nil nil nil nil))
        (assoc :se (QuadTreeNode. (+ x hw) (+ y hh) hw hh nil nil nil nil nil))
        (insert pt))))
  
  (list-all [this]
    (if (= (:data this) ::branches)
      ; get children's
      (->> (get-branches this)
        (map list-all)
        (flatten)
        (filter identity))
      (:data this)))
  
  (delete [this {x :x, y :y, :as pt}]
    (when (out-of-bounds this pt)
      (throw (IllegalArgumentException. "Point is out of the QuadTree's bounds")))
    
    (cond
      ; if split, remove from the correct branch, then check to heal
      (isa? (:data this) ::branches)
        (let [qd (which-quadrent this pt)]
          (merge-split (assoc this qd (delete (get this qd) pt))))

      ; if it is the same point, delete it      
      (= pt (:data this)) (assoc this :data nil)

      ; nothing to do, 
      :else this))
  
  (merge-split [this]
    (cond 
      (= (:data this) ::branches)
      (do ;; travel to the bottom first
        (apply-to-branches this merge-split)

        ; check yourself
        (let [data (filter identity (apply-to-branches this :data))]
          (if (or (not-empty (rest data))  ;; if more than one branch 
              (= ::branches (first data))) ;; or contains branches
            this
            (assoc (delete-all this) :data (first data)))))))
     
      ; merge if only non-nil branch and it itself isn't a branch
      #_(if (or (not-empty (rest data)) (isa? ::branches (first data))) this
        (assoc (delete-all this) :data (first data)))  
  
  (delete-all [this] 
    (QuadTreeNode. (:x this) (:y this) (:w this) (:h this) nil nil nil nil nil))
  
  (query [this pt] 
    (if (not (out-of-bounds this pt))
        (if (not (= (:data this) ::branches)) this
          (query (get-quadrent this pt) pt)))))

;;;;;;;;;;;;;;;;;;;
;;  Constructor  ;;
;;;;;;;;;;;;;;;;;;;

(defn quadtree
  "create a quadtree"
  [x y w h]
  (QuadTreeNode. x y w h nil nil nil nil nil))