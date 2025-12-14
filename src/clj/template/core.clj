(ns template.core
  (:require [quil.core :as q]
            [template.dynamic :as dyn]))

(def w 900)
(def h 900)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 20)
  (q/no-fill)
  {})

(defn draw [_])

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw state))
  (println "Done")
  (q/no-loop))

(comment
  (reset! dyn/draw-height h)
  (reset! dyn/draw-width w)
  (dyn/sketch #'draw))
