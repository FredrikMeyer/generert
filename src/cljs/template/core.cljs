(ns template.core
  (:require
   [quil.core :as q :include-macros true]
   [quil.middleware :as m]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 20)
  (q/no-fill)
  (q/frame-rate 60)
  {})

(defn draw []
  (let [t (q/frame-count)
        hue (mod (+ 20 (* 0.5 t)) 100)
        x (+ (/ w 2) (* 200 (q/sin (/ t 20))))
        y (+ (/ h 2) (* 200 (q/cos (/ t 25))))]
    (q/background 0)
    (q/stroke hue 80 100 90)
    (q/fill hue 80 100 60)
    (q/ellipse x y 140 140)))

(q/defsketch cljs-template
  :host "app"
  :size [w h]
  :setup setup
  :draw draw
  :middleware [m/fun-mode])
