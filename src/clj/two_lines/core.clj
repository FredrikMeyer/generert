(ns two-lines.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.lines :as gtl]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 10)
  {})

(def base-line-1
  (fn [t]
    (gtl/point-on-line [50 650] [850 850] t)))

(def base-line-2
  (fn [t]
    (gtl/point-on-line [100 50] [850 450] t))
  )


(defn random-points-on-line [p1 p2 n]
  (dotimes [_ n]
    (let [pnt (gtl/point-on-line p1 p2 (rand))]
      (apply q/point pnt)
      )
  ))

(defn draw []
  (comment
    (doseq [t (range 0 1 0.01)]
      (apply q/point (gtl/point-on-line [100 100] [300 300] t))
      ))

  (dotimes [_ 1000]
    (let [t (rand)
          pt-1 (base-line-1 t)
          pt-2 (base-line-2 (- 1 t))]
        (random-points-on-line pt-1 pt-2 500)

        ))
  )

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

(defn save-on-click [state event]
  (println "Saved")
  (println state)
  (q/save-frame (str "two-lines" (hash state) "_" (q/random 0 1) ".tif"))
  state)

(defn redraw [old-state event]
  (if (= (:key event) :r)
    (q/redraw))
  old-state)

(q/defsketch two-lines
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked save-on-click
  :key-pressed redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
