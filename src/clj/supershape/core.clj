(ns supershape.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def w 800)
(def h 800)
 
(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/no-fill) 
  {})

(def n1 (/ 1 3))
(def n2 1.333)
(def n3 0.333)
(def m (/ 19 6))
(def a 1)
(def b 1) 

(defn shape-with-params [n1 n2 n3 m a b]
  (fn [angle]
    (let [left-part (Math/pow (Math/abs (* (/ 1 a) (Math/cos (/ (* m angle) 4)))) n2)
        right-part (Math/pow (Math/abs (* (/ 1 b) (Math/sin (/ (* m angle) 4)))) n3)]
    (/ 1 (Math/pow (+ left-part right-part) (/ 1 n1))))))

(defn draw-super-shape []
  (doseq [
          ;; n1 (range 0.1 1.5 0.2)
         n2 (range 0.1 1.5 0.1)
         m (range 1 10 1)
          ]
    (q/with-stroke [(q/map-range m 1 10 30 70) 70 100]
      (let [s-shape (shape-with-params n1 n2 1 m 1 1)]
        (q/begin-shape)
        (doseq [a (range 0 (* 12 q/PI) 0.001)]
          (let [r (s-shape a)
                x (* 300 r (Math/cos a))
                y (* 300 r (Math/sin a))]
            (q/vertex x y) 
            )
          )
        (q/end-shape :close)
        ))))

(defn draw []
  ;; Your drawing here
  (q/translate (* 0.5 w) (* 0.5 h))
  (draw-super-shape)
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
  (q/save-frame (str "supershape" (hash state) "_" (q/random 0 1) ".tif"))
  state)

(defn redraw [old-state event]
  (if (= (:key event) :r)
    (q/redraw))
  old-state)

(q/defsketch supershape
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked save-on-click
  :key-pressed redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
