(ns color-circle.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as t]))

(def w 600)
(def h 600)

(defn setup []
  (q/color-mode :hsb 360 w h 100)
  (q/no-stroke)
  (q/stroke-weight 0)
  {:segment-count 45
   :saturation 0
   :brightness 0})

(defn draw [state]
  (q/begin-shape :triangle-fan)
  (q/vertex (/ w 2) (/ h 2))
  (let [angle-step (/ 360 (:segment-count state))
        radius 250]
    (doseq [phi (range 0 (+ 360 angle-step) angle-step)]
      (let [vx (+ (/ w 2) (* radius (Math/cos (q/radians phi))))
            vy (+ (/ h 2) (* radius (Math/sin (q/radians phi))))]
        (q/vertex vx vy)
        (q/fill phi (:saturation state) (:brightness state)))))

  (q/end-shape))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 360)
  (draw state)
  ;; (println "Done")
  ;; (q/no-loop)
  )

(defn mouse-moved [state event]
  (-> state
      (assoc :saturation (:x event))
      (assoc :brightness (:y event))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore color-circle
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-moved mouse-moved
    :mouse-clicked (t/save-on-click-handler "color-circle")
    :key-pressed t/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
