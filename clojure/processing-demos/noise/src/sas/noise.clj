(ns sas.noise
  (:use [rosado.processing]
        [rosado.processing.applet]))

(defapplet noise-applet :title "Noise"
  :size [640 480]
  :setup (fn []) 
  :draw (fn []
          (dotimes [x 640] (dotimes [y 480]
            (set-pixel x y (color-int (rand-int 256)))))
          (Thread/sleep 10)))

(defn -main [& args] (run noise-applet))

