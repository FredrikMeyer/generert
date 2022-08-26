(ns alignment-in-grid.alignment-in-grid
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as dr]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :rgb)
  (q/stroke-weight 0)
  (q/stroke-cap :round)
  {:tile-count 40})

(defn get-color []
  (->> [[252 248 118] [55 141 164] [55 141 164] [228 228 228]]
       (rand-nth)
       (apply q/color)))

(defn distance-from-center [x y]
  (let [dx (- (/ w 2) x)
        dy (- (/ h 2) y)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn normalized-distance [x y]
  (let [dist (distance-from-center x y)]
    (- 15 (q/map-range dist 0 (/ w 2) 0 10))))

(defn draw [{tile-count :tile-count}]
  (q/background 0)
  ;; (q/stroke 100 100)
  (doseq [gridX (range 0 tile-count)
          gridY (range 0 tile-count)]
    (let [posX (* gridX (/ w tile-count))
          posY (* gridY (/ h tile-count))
          toggle (rand-int 2)]
      (if (= toggle 0)
        (do
          ;; (q/stroke-weight (* 5 (q/noise (/ (* posX posY) 500))))
          ()
          ;; (q/stroke-weight 15)
          (q/stroke-weight (normalized-distance posX posY))
          (q/stroke (get-color))
          (q/line posX posY (+ posX (/ w tile-count)) (+ posY (/ h tile-count))))
        (do
          (q/stroke-weight 5)
          (q/stroke 255)
          (q/line posX (+ posY (/ h tile-count)) (+ posX (/ h tile-count)) posY))))))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 360)
  (draw state)
  ;; (println "Done")
  (q/no-loop))

(defn save-on-click [state _]
  (println "Saved")
  (println state)
  (q/save-frame (str "alignment_in_grid" (hash state) "_" (q/random 0 1) ".jpg"))
  state)

(defn mouse-moved [state event]
  (-> state
      (assoc :saturation (:x event))
      (assoc :brightness (:y event))))

(q/defsketch #_:clj-kondo/ignore pixels
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-moved mouse-moved
  :mouse-clicked (dr/save-on-click-handler "pixels")
  :key-pressed dr/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])


