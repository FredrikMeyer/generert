(ns sketches.random-lines-circle
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as tools]))

(def w 600)
(def h 600)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/background 0)
  (q/stroke 100 20)
  (q/stroke-weight 1)
  (q/no-fill)
  {:radius (- (/ w 2) 100)
   :n 1000 ;; 20000 på stort bilde
   :left-center 0
   :right-center (/ q/PI 3)  ;; pi/4 1
   })

(defn update-state [state]
  state)

(defn rgauss [mean std]
  (+ mean (* std (q/random-gaussian))))

(defn random-point-on-circle [x y radius mean]
  (let [theta (rgauss mean (* 0.35 q/PI))]  ;; 0.3
    [(+ x (* radius (Math/cos theta)))
     (+ y (* radius (Math/sin theta)))]))

(defn random-line-on-circle [x y radius mean]
  (let [[a b] (random-point-on-circle x y radius mean)
        [c d] (random-point-on-circle x y radius mean)]
    (q/line a b c d)))

(defn draw-state [state]
  (q/background 0)
  (let [cx (* 0.5 (q/width))
        cy (* 0.5 (q/height))
        radius (:radius state)]
    (dotimes [_ (:n state)]
      (random-line-on-circle cx cy radius (:left-center state))
      (random-line-on-circle cx cy radius (:right-center state))))
  (println "Done")
  (q/no-loop))

;; ide: lage stjerner i bakgrunnen??

(defn redraw []
  (q/redraw))

(defn save-on-click [state event]
  (println "savedq")
  (println state)
  (q/save-frame (str "s" (hash state) "_" (q/random 0 1) " .jpg"))
  state)

(defn sketch []
  (q/defsketch quil-drawings
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (tools/save-on-click-handler "random-lines-circle")
    :key-pressed tools/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
