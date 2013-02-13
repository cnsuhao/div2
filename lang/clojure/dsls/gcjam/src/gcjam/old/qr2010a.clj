(ns gcjam.qr2010a
  (:use [gcjam.parser :only (*rootpath* defparser get-path parse-line)]
        [clojure.contrib.string :only (replace-char split split-lines trim)]))

;; ===============
;;  Snapper Chain
;; =============== 
;;
;; Problem
;; ----------------
;; The Snapper is a clever little device that, on one side, plugs its input 
;; plug into an output socket, and, on the other side, exposes an output socket
;; for plugging in a light or other device.
;; 
;; When a Snapper is in the ON state and is receiving power from its input plug, 
;; then the device connected to its output socket is receiving power as well. 
;; When you snap your fingers -- making a clicking sound -- any Snapper receiving 
;; power at the time of the snap toggles between the ON and OFF states.
;;
;; In hopes of destroying the universe by means of a singularity, I have purchased 
;; N Snapper devices and chained them together by plugging the first one into a 
;; power socket, the second one into the first one, and so on. The light is plugged 
;; into the Nth Snapper.
;; 
;; Initially, all the Snappers are in the OFF state, so only the first one is 
;; receiving power from the socket, and the light is off. I snap my fingers once, 
;; which toggles the first Snapper into the ON state and gives power to the second 
;; one. I snap my fingers again, which toggles both Snappers and then promptly cuts 
;; power off from the second one, leaving it in the ON state, but with no power. I 
;; snap my fingers the third time, which toggles the first Snapper again and gives 
;; power to the second one. Now both Snappers are in the ON state, and if my light 
;; is plugged into the second Snapper it will be on.
;; 
;; I keep doing this for hours. Will the light be on or off after I have snapped my 
;; fingers K times? The light is on if and only if it's receiving power from the 
;; Snapper it's plugged into.
;;
;; -0 -0 -0
;; +1 +0 -0
;; -0 -1 -0
;; +1 +1 +0
;; -0 -0 -1
;; 
;;
;; Input
;; ------------------
;; The first line of the input gives the number of test cases, T. T lines follow. 
;; Each one contains two integers, N and K.
;;
;;
;; Output
;; ------------------
;; For each test case, output one line containing "Case #x: y", where x is the case 
;; number (starting from 1) and y is either "ON" or "OFF", indicating the state of 
;; the light bulb.
;;
;;
;; Limits:
;;     1 <= T <= 10,000
;;
;; Small dataset:
;;     1 <= N <= 10
;;     0 <= K <= 100
;;
;; Large dataset:
;;     1 <= N <= 30
;;     0 <= K <= 108
;;
;; ======================
;;         Sample
;; ======= ==============
;;  Input      Output 
;; ======= ==============
;;  4       Case #1: OFF
;;  1 0     Case #2: ON
;;  1 1     Case #3: OFF
;;  4 0     Case #4: ON
;;  4 47   
;; ======= ==============

;; calc-snap
;;
;; light is only on when all the snappers are lit up 
;; the state of any given snapper is the same as binary counting
;; there is an overflow if >= 2^n
;; so, toggled state is k % 2^n
;; and full circuit if every bit is 1 (2^n-1)
(defn calc-snap
  "Calculate the state of the light with N snappers and K snaps" 
  [[n k]]
  (let [p (Math/pow 2 n) ]
    (= (- p 1) (mod k p))))

(def *caseout* "Case #%d: %s")

(def format-output #(format *caseout* %1 (if %2 "ON" "OFF")))

;; input file parser 
(defparser snapper-parse
  (snap [int n, int k] nil 
    [n k])
  (init [int numtests] [lines] 
     (for [l lines]
       (parse-line snap l))))

(defn run
  [tst]
  (let [ pth (get-path *rootpath* tst "in") ]
    (map (fn [t c] (format-output c (calc-snap t))) (snapper-parse pth) (iterate inc 1) )))

(defn write
  [tst]
 (let [ pth (get-path *rootpath* tst "out") ]
   (spit pth (trim (apply str (map #(format "%s\n" %) (run tst)))))))