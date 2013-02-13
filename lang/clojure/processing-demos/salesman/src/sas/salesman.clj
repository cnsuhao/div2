(ns sas.salesmen
  (:use [quil.core]))

;;;;; Globals
(def WIDTH 400)
(def HEIGHT 400)
(def RADIUS 10)

(def RAND_POP_SIZE 5)
(def MUTATE_POP_SIZE 10)
(def N_TOWNS 50)

(def path-atom (atom nil))


;;;;; Helpers
;; generate towns that are within the bounds
(defn distance
  [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))
  
(defn path-distance
  [path]
  (reduce + (map distance path (rest (cycle path)))))


(defn mutate-path
  [path]
  (let [c (count path)
        i (rand-int c)
        j (rand-int c)
        a (nth path i)
        b (nth path j)]
    (-> (vec path)
        (assoc i b)
        (assoc j a))))

;;;;; Simulation
(defn random-town []
  [(+ RADIUS (rand-int (- WIDTH RADIUS RADIUS))) (+ RADIUS (rand-int (- HEIGHT RADIUS RADIUS)))])

(defn generate-towns
  [n]
  (loop [towns []]
    (if (= n (count towns))
      towns
      (recur
        (let [town (random-town)
              dists (map #(distance town %) towns)]
          (if (some #(<= % (* 2 RADIUS)) dists)
            towns
            (conj towns town)))))))


(defn reset-towns []
  (reset! path-atom (generate-towns N_TOWNS)))

(defn step []
  (let [rand (repeatedly RAND_POP_SIZE #(shuffle @path-atom))
        mutations (repeatedly MUTATE_POP_SIZE #(mutate-path @path-atom))
        paths (sort-by path-distance (concat [@path-atom] rand mutations))]
    (reset! path-atom (first paths))))


;;;;; Applet

(defn setup []
  (reset-towns)
  
  (smooth)
  (frame-rate 10))

(defn draw []
  (step)
  
  (background-float 100)
  
  ; draw towns
  (stroke-weight 2)
  (stroke-float 80)
  (fill-float 255)
  (doseq [[x y] @path-atom]
    (ellipse x y (* 2 RADIUS) (* 2 RADIUS)))
  
  ; draw path
  (stroke-float 0)
  (stroke-weight 2)
  (doseq [[[x1 y1] [x2 y2]] (map vector @path-atom (rest (cycle @path-atom)))]
    (line x1 y1 x2 y2)))

(defn mouse-clicked []
  (reset-towns))

(defsketch salesmen-applet
  :title "Traveling Salesmen"
  :size [WIDTH HEIGHT]
  :setup setup
  :draw draw
  :mouse-clicked mouse-clicked)

(defn -main [] )
