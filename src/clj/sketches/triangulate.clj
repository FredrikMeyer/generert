(ns sketches.triangulate
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]
   [tools.shapes :as s]
   [fastmath.random :as fr]
   [tools.delaunay :as del]))

(def w 700)
(def h 700)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  (q/ellipse-mode :radius)
  {})

(defn random-pt []
  (let [x (fr/grand (* 0.5 w) 100)
        y (fr/grand (* 0.5 w) 100)]
    (s/point x y)))

(defn draw []
  (q/background 100)
  (q/stroke 0 100)
  (let [pts (repeatedly 1000 random-pt)
        ;; pts (r/random-pts 400 [0 w] [0 h])
        triang (del/triangulate pts)]
    #_(do
      (println pts)
      (println triang))
    (doseq [t triang]
      (q/with-fill [20 100 100 50]
        (s/draw t)))
    #_(doseq [[i x y] (map-indexed (fn [ind [x y]] [ind x y]) pts)]
      (q/with-fill [40 100 100]
        (q/ellipse x y 5 5)
        (q/with-fill [0 100]
          (q/text (str i) x y))))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore chaikin
    :title "delaunay"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "delaunay")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

