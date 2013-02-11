(ns sas.bitops
  (:use [sas.bitops.enum :only (defenum)]))

(defn bitmap
  "Creates a bitmap"
  [& bits]
  (reduce bit-or 0 bits))

(defn nbits
  "How many bits to formed a given unsigned number"
  [N]
  (inc (int (Math/floor (/ (Math/log N) (Math/log 2))))))

(defn bitmask
  "Creates a bitmask for a given max number"
  [N]
  (dec (bit-shift-left 1 N)))

(defn bitmask-seq
  "Takes a bitmap and converts it into a vector"
  ([value nbits] (bitmask-seq value nbits (bitmask nbits)))
  ([value nbits mask] 
  (lazy-seq
    (when (not (= value 0))
      (cons (bit-and mask value) (bitmask-seq (bit-shift-right value nbits) nbits mask))))))