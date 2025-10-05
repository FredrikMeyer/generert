(ns sketches.circumcircles
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]
   [tools.points :as p]
   [tools.shapes :as s]
   [tools.delaunay :as del]
   [tools.random :as r]))

(def w 700)
(def h 700)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  (q/ellipse-mode :radius)
  {})

(defn random-triangle [[a b] [c d]]
  (let [[a b c] (r/random-pts 3 [a b] [c d])]
    (s/->Triangle a b c)))

(defn draw []
  (doseq [n (range 10 w 70)
          m (range 10 h 70)]
    (let [triangle (random-triangle [n (+ n 50)] [m (+ m 50)])
          circumcircle (del/circumcircle triangle)
          {[x y] :center r :radius} circumcircle
          {[ax ay] :a [bx by] :b [cx cy] :c} triangle]
      (q/with-fill [0 0 100 50]
        (q/ellipse x y r r))
      (q/with-fill [3 100 100]
        (q/triangle ax ay bx by cx cy)))))

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
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "circumcircles")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

