(ns gcjam.test
  (:use [gcjam.private]))



; (defgrammar q2008b
;   :file => :num-cases :case!num-cases
;   :num-cases => (int.v) :as v
;   :case => (int.num-lines)
;            (int.NA int.NB)
;            timetables!num-lines
;   :timetables => (datetime.dep datetime.arr)
;   :datetime   => int.hour ":" int.min :as [ hour min ])

(defn indices-of
  [v coll]
  (filter #(not (nil? %)) (map-indexed (fn [i a] (if (= a v) i)) coll)))



(defn by-pairs
  [coll]
  (letfn [ (take-pair [c] (when (next c) (take 2 c))) ]
    (lazy-seq
      (when-let [pair (seq (take-pair coll))]
        (cons pair (by-pairs (rest coll)))))))


(def grmr (with-meta '(
                        
   :file => num-cases case!num-cases
   :num-cases => (int.v) :as v
   :case => (int.num-lines)
            (int.NA int.NB)
            timetables!num-lines
   :timetables => (datetime.dep datetime.arr)
   :datetime   => int.hour ":" int.min :as [ hour min ]
   
) (assoc (meta #'grmr) :grammar  true))) 
