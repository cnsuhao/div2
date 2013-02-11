(ns sas.bitops.enum)

(defn- parse-enum-arg [v props]
  "Figures out where to put a given enum argument"
  (cond 
    (or (= v 'bitmap) (number? v)) (assoc props :type v)
    (or (string? v) (symbol? v)) (assoc props :prefix (str v))
    (or (vector? v) (seq? v)) (assoc props :attr v)
    :else props))

(defn- parse-enum-args [args props]
  "Loops through each enum argument, processing as it goes"
  (if (empty? args)
    props
    (parse-enum-args (rest args) (parse-enum-arg (first args) props))))

(defn- pairs
  [coll]
  (if (empty? (rest coll)) coll
    (map (fn [a b] [a b]) (drop-last coll) (rest coll))))

(defn- build-attrs
  [props]
  (let [{typ :type, prfx :prefix} props
        attr (conj (vec (:attr props)) nil)
        start (if (= typ 0) 0 1)
        func (if (number? typ) inc #(bit-shift-left % 1))]
    (-> (:attr props)
      (vec)
      (conj nil)
      (pairs)
      (->> 
        (filter #(not (number? (first %))))
        (reduce 
          (fn [[coll step] [a b]]
            (let [v (if (number? b) b step)]
              [(conj coll [(symbol (str prfx a)) v]) (func v)])) 
          [[] start])
        (first)
        (assoc props :attr)))))

(defn- extend-attr
  "add some extra properties for convinience"
  [props]
  (let [attr (:attr props)
        max-i (apply max (map second attr))
        pow (Math/floor (/ (Math/log max-i) (Math/log 2)))
        mask (dec (bit-shift-left 1 (inc pow)))]
    (if (and (= (:type props) 'bitmap) (> (.length (:prefix props)) 0))
      (assoc props :attr (conj attr [(symbol (str (:prefix props) "MASK")) mask]))
      props)))

(defn- enum-defs
  [attr]
  `(do ~@(for [[s i] attr] `(def ~s ~i))))


(def #^{:private true} defaults { :type 0, :prefix "", :attr [] })

(defmacro defenum
  [& args]
  (let [props (parse-enum-args args defaults)]
    (when (not (empty? (:attr props)))
      (-> props
        (build-attrs)
        (extend-attr)
        (:attr)
        (enum-defs)))))