(ns texture.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def w 900)
(def h 900)

(def cx (/ w 2))
(def cy (/ h 2))
 
(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 20)
  (q/no-fill)
  {})

(defn random-point-circle [r]
  (let [theta (q/random 0 (* 2 q/PI))
        x (+ cx (* r (Math/cos theta)))
        y (+ cy (* r (Math/sin theta)))]
    [x y]
    ))

(defn random-line-circle [r]
  (let [p1 (random-point-circle r)
        p2 (random-point-circle r)]
    (q/line p1 p2)
    )
  )

(defn central-piece [diff max-radius n-lines]
  (doseq [r (range 50 max-radius diff)] 
    (doseq [_ (range 1 n-lines)]
      (random-line-circle r)
      ))
  )


(defn draw []
  (comment 
    (q/with-translation [-220 -220]
      (central-piece 25 201 500))

    (q/with-translation [220 -220]
      (central-piece 50 201 500))

    (q/with-translation [-220 220]
      (central-piece 25 201 200))

    (q/with-translation [220 220]
      (central-piece 50 201 200)))

;  (q/stroke 100 10)
  (central-piece 30 450 500)  ;; 100 450 500
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
  (q/save-frame (str "texture" (hash state) "_" (q/random 0 1) ".tif"))
  state)

(defn redraw [old-state event]
  (when (= (:key event) :r)
    (q/redraw))
  old-state)

(q/defsketch texture
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked save-on-click
  :key-pressed redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
