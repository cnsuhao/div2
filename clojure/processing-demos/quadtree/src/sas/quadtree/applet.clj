(ns sas.quadtree.applet
  (:import [java.awt BasicStroke Dimension])
  (:use [rosado.processing]
        [rosado.processing.applet]
        [sas.quadtree]))

(declare qtree-applet)

(def WIDTH (int 400))
(def HEIGHT (int 400))
(def OFFX (atom (int 0)))
(def OFFY (atom (int 0)))
(def SCALE (atom (int 1)))

(def C_BASE 32)
(def C_CAP 200)
(def C_DEPTH 10)

(def K_ESC 27)
(def K_TILDA 192)
(def K_SPACE 32)

(def SPRAY 15)
(def DURATION 12)

(def *quadtree* (atom (quadtree 0 0 WIDTH HEIGHT)))
(def *esc* (atom false))
(def *queued-pts* (atom []))
(def *highlight* (atom nil))
(def *mpressed* (atom false))
;(def *all-pts* (atom {}))

(defn update-transform []
  (let [W (.width *applet*) H (.height *applet*)
        rW (/ (float W) WIDTH)
        rH (/ (float H) HEIGHT)]
	  
	  (if (< rW rH)
     (do
       (reset! OFFX 0)
       (reset! OFFY (* 0.5 (- H (* rW HEIGHT))))
       (reset! SCALE rW)) 
     (do
       (reset! OFFX (* 0.5 (- W (* rH WIDTH))))
       (reset! OFFY 0)
       (reset! SCALE rH)))))

(defn easing
  [t a b d]
  (+ a (* (- b a) (/ (float t) (float d)))))

(defn get-nodes
  "Calculates now many nodes actually have points stored in them"
  [tree]
  (if (not (= (:data tree) :sas.quadtree/branches)) [tree] 
    (->> (get-branches tree)
      (filter identity)
      (map get-nodes)
      (flatten)
      (cons tree))))
  
(defn setup []
  ;; set background color
  (background 0)
  (no-stroke)
  (smooth)
)

(defn draw []
  ;; quit if we have to
  (if @*esc* (stop *applet*))
   
  ; grab points
  (let [pts @*queued-pts*]
    (reset! *queued-pts* [])
    (let [qt @*quadtree*]
      (reset! *quadtree* (reduce (fn [qt [x y]] (insert qt x y)) qt pts)))) 
    
  (update-transform)
  
  (reset-matrix)
  (translate @OFFX @OFFY)
  (scale @SCALE)
  (no-stroke)
  (fill 255)
  (rect 0 0 WIDTH HEIGHT)
  
  (let [Xi (.mouseX *applet*) Yi (.mouseY *applet*)
        X (/ (- Xi @OFFX) @SCALE) Y (/ (- Yi @OFFY) @SCALE)]
    (reset! *highlight* (query @*quadtree* X Y)))
  
  ; draw the boxes
  (stroke 255)
  (stroke-weight 0.5)
  (doseq [{x :x y :y w :w h :h :as n} (get-nodes @*quadtree*)]
    (let [f (cond (and @*mpressed* (= n @*highlight*)) 0
                  (= n @*highlight*) 255
                  :else (easing (int (/ WIDTH w 2)) 24 64 C_DEPTH))]        
      (fill f)  
      (rect x y w h)))
  
  ; draw points
  (stroke-weight 0.25)
  (stroke 0)
  (fill 222)
  (doseq [{x :x y :y} (list-all @*quadtree*)]
    (when (and x y)
      (ellipse x y 3 3)))
  
  (Thread/sleep 10))

(defn mouse-clicked [evt]
  (let [Xi (.getX evt) Yi (.getY evt)
        X (/ (- Xi @OFFX) @SCALE) Y (/ (- Yi @OFFY) @SCALE)]
    (when (and (>= X 0) (<= X WIDTH) (>= Y 0) (<= X HEIGHT))
      (println  (swap! *queued-pts* conj [(float X) (float Y)])))))

(defn key-pressed [evt]
  (cond 
    (= K_ESC (.getKeyCode evt)) (reset! *esc* true)
    (= K_TILDA (.getKeyCode evt)) (dotimes [_ SPRAY] (swap! *queued-pts* conj [(rand-int WIDTH) (rand-int HEIGHT)])) 
    (= K_SPACE (.getKeyCode evt)) (swap! *quadtree* delete-all)))

(defapplet qtree-applet :title "QuadTree"
  :size [640 480]
  :setup setup 
  :draw draw
  :key-pressed key-pressed
  :mouse-clicked mouse-clicked
  :mouse-pressed (fn [evt] (reset! *mpressed* true))
  :mouse-released (fn [evt] (reset! *mpressed* false)))

(defn -main [& args] (run qtree-applet))
