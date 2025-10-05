(ns sketches.19052024
    (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.chaikin :as c]
   [tools.drawing :as d]))
(def w 800)
(def h 800)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/no-fill)
  {})

(defn draw []
  (let [lower-curve [[200 600] [400 500] [600 200] [200 200] ]
        chaik (c/chaikin-curve lower-curve 10)]
    (q/begin-shape :points)
    (doseq [a chaik]
      (apply q/vertex a)
      #_(q/line a b))
    (q/end-shape :close)))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore mysketch
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "template")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
