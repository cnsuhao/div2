(ns sas.astar
  (:use [rosado.processing]
        [rosado.processing.applet]))

;;;;;;;;;;;;;;
;;  CONFIG  ;;
;;;;;;;;;;;;;;

; general
(def WIDTH 480)
(def HEIGHT 320)
(def SCALE 0.01)
(def STEPS 1)

(def diag (Math/sqrt 2))

(def bg (atom nil))
(def visited (atom nil))
(def start (atom nil))
(def goal (atom nil))

(def *state* (atom nil))
(def *solution* (atom nil))

(defstruct astar-path :pt :prev :cost :estimate)

(defn perlin-noise
  [x y]
  (let [v (noise (* x SCALE) (* y SCALE))]
        (* v v v)))

(defn get-neighboring
  [[x y]]
  (set 
    (-> []
      (conj [(max (dec x) 0) (max (dec y) 0)])
      (conj [(max (dec x) 0) y])
      (conj [(max (dec x) 0) (min (inc y) (dec HEIGHT))])
      (conj [x (max (dec y) 0)])
      (conj [x (min (inc y) (dec HEIGHT))])
      (conj [(min (inc x) (dec WIDTH)) (max (dec y) 0)])
      (conj [(min (inc x) (dec WIDTH)) y])
      (conj [(min (inc x) (dec WIDTH)) (min (inc y) (dec HEIGHT))]))))

(defn no-loops?
  [pth pt]
  (loop [pth (:pts pth)]
    (if (empty? pth) true
      (if (= pt (first pth))
        false
        (recur (rest pth))))))

(defn calculate-cost
  [[x0 y0] [x1 y1]]
  (let [h0 (perlin-noise x0 y0)
        h1 (perlin-noise x0 y0)
        dh (* 1 (- h1 h0))
        cost (+ 2 (* (abs dh) dh))]
    (if (< 1 (abs (+ (- x1 x0) (- y1 y0))))
      (* diag cost)
      cost)))    

(defn estimate-remainder
  "Estimate the remainder by using manhattan distance.  The estimate is always less 
   than actual cost due to 'base-cost'."
  [[x0 y0]]
  (let [[x1 y1] @goal
        dx (abs (- x1 x0))
        dy (abs (- y1 y0))]
    (+ dx dy)))  
    
(defn not-in-path?
  [pth pt]
  (if (empty? pth) true
    (if (= pt (:pt pth)) false
      (recur (:prev pth) pt))))

(defn extend-path
  ([pth]
    (let [ pts (get-neighboring (:pt pth))
           valid-pts (filter (fn [pt] (not-in-path? pth pt)) pts) ]
      (map (fn [pt] (extend-path pth pt)) valid-pts)))
  ([pth pt]
    (let [ prv (:pt pth)
          dist (calculate-cost prv pt)]
      (struct astar-path pt prv (+ (:cost pth) dist) (estimate-remainder pt)))))

(defn total-cost
  [pth]
  (+ (:cost pth) (:estimate pth)))

(defn astar-step 
  [[open closed]]
  (let [current (first open)
        next-paths (extend-path current)
        open (rest open)
        closed (assoc closed (:pt current) current)]
    (let [[x y] (:pt current)]
      (.set @visited x y (color 255 0 0)))
    (loop [nxt next-paths open open closed closed]
      (if (empty? nxt) 
        [(sort (fn [a b] (< (total-cost a) (total-cost b))) open) closed]
        (let [pth (first nxt)
              pt (:pt pth)]
          (if (contains? closed pt)
            (if (< (total-cost pth) (total-cost (closed pt)))
              (recur (rest nxt) open (assoc closed pt pth))
              (recur (rest nxt) open closed))
            (do
              (.set @visited (first pt) (second pt) (color 255 255 0))
              (recur (rest nxt) (conj open pth) closed))))))))

(defn update-astar
  ([state] (update-astar STEPS state))
  ([c state]
    (if (zero? c) state
      (let [ pt (:pt (first (first state))) ]
        (if (= pt @goal)
          (reset! *solution* (first (first state))))
          (recur (dec c) (astar-step state))))))

(defn create-background []
  (let [img (create-image WIDTH HEIGHT ARGB)]
    (doseq [x (range WIDTH) y (range HEIGHT) ]
      (.set img x y (color (* 255 (perlin-noise x y) ))))
    (reset! bg img)))

(defn reset-simulation []
  ; seed
  (.noiseDetail *applet* (int 4) (float 0.5))
  (noise-seed 0); (rand-int Integer/MAX_VALUE))
  
  ; images
  (create-background)
  (reset! visited (create-image WIDTH HEIGHT ARGB))
  
  ; start and goal points
  (let [ a (+ (rand-int 40) 40)
         b  (+ (rand-int 40) 40)
         c (- WIDTH (+ (rand-int 40) 40))
         d  (- HEIGHT (+ (rand-int 40) 40))
         [x0 x1] (if (< (rand) 0.5) [a c] [c a])
         [y0 y1] (if (< (rand) 0.5) [b d] [d b]) ]
    (reset! start [x0 y0])
    (reset! goal [x1 y1]))

  ; reset state
  (reset! *state* [ [(struct astar-path @start nil 0 0) ] {} ])
  (System/gc))

(defn setup [] 
  ; setup colors
  (def closed-pt (color 255 0 0))
  (def open-pt (color 255 255 0))
  (def path-pt (color 255 0 255))
  (def goal-pt (color 0 255 0))
  
  ; setup graphics
  (no-stroke)
  (reset-simulation))

(defn step []
  ;; update 
  (reset! *state* (update-astar @*state*))
  
  ;; draw background
  (no-stroke)
  (background 0)
  (image @bg 0 0)
  (blend @visited 0 0 WIDTH HEIGHT 0 0 WIDTH HEIGHT 1)
  
  ;; draw start and end points
  (let [[x0 y0] @start
        [x1 y1] @goal]
    (stroke 0)
    (if (nil? @*solution*)
      (fill-int path-pt)
      (fill-int goal-pt))
    (rect (- x0 2) (- y0 2) 5 5)
    (fill-int goal-pt)
    (rect (- x1 2) (- y1 2) 5 5)))

(defapplet astar :title "A* Path Finding"
  :size [WIDTH HEIGHT]
  :setup setup :draw step)

(defn -main [& args] (run astar))
