(ns sas.termites
  (:use [rosado.processing]
        [rosado.processing.applet]))

;;;;;;;;;;;;;;
;;  CONFIG  ;;
;;;;;;;;;;;;;;

; general
(def WIDTH 320)
(def HEIGHT 240)
(def SCALE 3)

(def STEPS 8)            ; steps per draw cycle
(def WORLD_WRAP true)    ; world wraps 
(def WOOD_PERCENT 0.25)  ; percentage of area which is wood

(def TRMT_COUNT 400)     ; how many termites
(def TRMT_TURN 0.25)     ; percent chance termite turns

; colors
(def CLEAR #(background 28))    ; background
(def BG_COLOR #(fill 28 28 28))  ; termite
(def TM_COLOR #(fill 220 0 0))  ; termite
(def TC_COLOR #(fill 220 220 0)) ; termite loaded
(def WD_COLOR #(fill 60 100 255))   ; wood
(def ST_COLOR #(fill 75 130 255)) ; stuck wood

; sticky
(def STICKY? true)
(def STICKY_IMAGE "data/termites_mask.jpg")
(def STICKY_TEST #(< % 0.65)) ; make the dark parts sticky

;;;;;;;;;;;;;
;;  STATE  ;;
;;;;;;;;;;;;;
(def *world* (make-array Integer/TYPE WIDTH HEIGHT))
(def *termites* (atom nil))

; state flags
(def WOOD 1)
(def TERMITE 2)
(def STICKY 4)
(def STUCK 8)

(def NORTH 0)
(def EAST 1)
(def SOUTH 2)
(def WEST 3)

;;;;;;;;;;;;;;;
;;  HELPERS  ;;
;;;;;;;;;;;;;;;
(defn luminance
  [v]
  (let [r (bit-shift-right (bit-and 16711680 v) 16)
        g (bit-shift-right (bit-and 65280 v) 8)
        b (bit-and 255 v)]
    (/ (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 255)))


;;;;;;;;;;;;;;;;;;;;;
;;  WORLD HELPERS  ;;
;;;;;;;;;;;;;;;;;;;;;

(def flag-set? (comp not zero? bit-and))

(defn has-wood?
  ([cell] (flag-set? WOOD cell))
  ([x y] (flag-set? WOOD (aget *world* x y))))

(defn has-termite?
  ([cell] (flag-set? TERMITE cell))
  ([x y] (flag-set? TERMITE (aget *world* x y))))

(defn is-sticky?
  ([cell] (flag-set? STICKY cell))
  ([x y] (flag-set? STICKY (aget *world* x y))))

(defn is-stuck?
  ([cell] (flag-set? STUCK cell))
  ([x y] (flag-set? STUCK (aget *world* x y))))

(defn place-wood
  [x y]
  (let [cell (bit-or WOOD (aget *world* x y))
        val (if (flag-set? STICKY cell) (bit-or STUCK cell) cell)]
  (aset *world* x y val)))

(defn place-termite
  ([[x y & _ :as termite]] (place-termite x y) termite)
  ([x y] (aset *world* x y (bit-or TERMITE (aget *world* x y)))))

(defn remove-wood
  [x y]
  (aset *world* x y (- (aget *world* x y) WOOD)))

(defn remove-termite
  ([[x y & _ :as termite]] (remove-termite x y) termite)
  ([x y] (aset *world* x y (- (aget *world* x y) TERMITE))))

(defn make-sticky
  [x y]
  (aset *world* x y (bit-or STICKY (aget *world* x y))))

;;;;;;;;;;;;;;;;
;;  TERMITES  ;;
;;;;;;;;;;;;;;;;
(defn rand-dir [] (dec (* 2 (rand-int 2))))

(defn turn
  ([[x y d c]] [x y (turn d (rand-dir)) c])
  ([dir amt]
    (mod (+ dir amt 4) 4)))

(defn random-turn
  [termite]
  (if (< (rand) TRMT_TURN)
    (turn termite)
     termite))

(defn move
  [[x y dir c :as termite]]
  (cond 
    (= dir NORTH) [ x (dec y) dir c]
    (= dir SOUTH) [ x (inc y) dir c]
    (= dir EAST)  [ (inc x) y dir c]
    (= dir WEST)  [ (dec x) y dir c]))

(defn clamp
  [[x y d c :as termite]]
  (if WORLD_WRAP
    [(mod (+ x WIDTH) WIDTH) (mod (+ y HEIGHT) HEIGHT) d c]
    [(min (dec WIDTH) (max x 0)) (min (dec HEIGHT) (max y 0)) d c]))

(defn collide
  [[x1 y1 d1 carrying :as trmt] [x0 y0 d0 _ :as orig]]
  (cond 
    (has-termite? x1 y1) orig
    (and (has-wood? x1 y1) (not carrying) (not (is-stuck? x1 y1))) (do (remove-wood x1 y1) [x1 y1 d1 true])
    (and (has-wood? x1 y1) carrying) (do (place-wood x0 y0) [x0 y0 (turn d0 2) false])
    :else trmt))

;;;;;;;;;;;;;
;;  WORLD  ;;
;;;;;;;;;;;;;
(defn init-wood
  []
  (let [npixels (* WIDTH HEIGHT) 
        c (* npixels WOOD_PERCENT)]
    (loop [c c]
      (if (not (zero? c))
        (let [x (rand-int WIDTH) y (rand-int HEIGHT)]
          (if (not (has-wood? x y))
            (do (place-wood x y) (recur (dec c)))
            (recur c)))))))

(defn init-sticky
  []
  (let [ image (load-image STICKY_IMAGE)
         cx (- (/ WIDTH 2) (/ (.width image) 2))
         cy (- (/ HEIGHT 2) (/ (.height image) 2))]
    (dotimes [x (.width image)]
      (dotimes [y (.height image)]
        (when (STICKY_TEST (luminance (.get image x y)))
            (make-sticky (+ x cx) (+ y cy)))))))


(defn init-termites
  []
  (loop [ termites [] c TRMT_COUNT ]
    (if (zero? c) (reset! *termites* termites)
      (let [x (rand-int WIDTH) y (rand-int HEIGHT)]
        (if (not (has-wood? x y))
          (recur (conj termites (place-termite [ x y (rand-int 4) false ])) (dec c))
          (recur termites c))))))

;;;;;;;;;;;;;;
;;  APPLET  ;;
;;;;;;;;;;;;;;
(defn get-color
  [x y]
  (cond 
    (has-termite? x y) TM_COLOR
    (and (has-wood? x y) (is-stuck? x y)) ST_COLOR
    (has-wood? x y) WD_COLOR
    :else BG_COLOR
    ))

(defn draw-point
  ([x y] (draw-point x y (get-color x y)))
  ([x y color]
    (color)
    (rect (* x SCALE) (* y SCALE) SCALE SCALE)))

(defn draw-screen
  []
  (CLEAR)
  (no-stroke)
  (dotimes [x WIDTH]
    (dotimes [y HEIGHT]  
      (when (not (zero? (aget *world* x y)))
        (draw-point x y)))))

(defn setup []
  (init-wood)
  (when STICKY? (init-sticky))
  (init-termites)
  (draw-screen))

(defn draw-termite
  [[x y _ carrying :as termite]]
  (if (and (has-termite? x y) carrying)
    (draw-point x y TC_COLOR)
    (draw-point x y)))

(def debug (fn [[x y & _ :as t]] (print x y "->") t))
(def eol (fn [[x y & _ :as t]] (println x y) t))
(defn update-termite
  [orig]
  (-> orig
    (remove-termite)
    (random-turn)
    (move)
    (clamp)
    (collide orig)
    (place-termite)
  ))

(defn update-termites
  ([termites] (update-termites STEPS termites))
  ([c termites]
    (if (zero? c) termites
      (let [updated (map update-termite termites)]
        (dorun updated)
        (recur (dec c) updated)))))

(defn step []
  (let [orig @*termites* 
        next (update-termites orig)]
    (doseq [termite orig] (draw-termite termite))
    (doseq [termite next] (draw-termite termite))
    (reset! *termites* next)))

;;;;;;;;;;;;
;;  MAIN  ;;
;;;;;;;;;;;;
(defapplet termites-applet :title "Termites"
  :size [(* WIDTH SCALE) (* HEIGHT SCALE)]
  :setup setup :draw step)

(defn -main [& args] (run termites-applet))
