(ns sketches.041025
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]
   [tools.points :as p]
   [tools.random :as r]))

(def w 700)
(def h 700)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  {})

(defn get-random-points [n]
  (for [_ (range n)]
    [(r/random 0 (- w 50))
     (r/random 0 (- h 50))]))

(defn random-color []
  [(r/random 0 100) 100 100])

(defn closest-to [pts pt]
  (loop [pts pts
         cand nil
         cand-dist nil]
    (if (empty? pts)
      cand
      (let [curr (first pts)]
        (if (nil? cand)
          (recur (rest pts)
                 curr
                 (p/distance-sq curr pt))
          (let [curr-dist (p/distance-sq curr pt)]
            (if (< curr-dist cand-dist)
              (recur (rest pts) curr curr-dist)
              (recur (rest pts) cand cand-dist))))))))

(defn draw []
  ;; (q/stroke 100)
  (q/stroke-weight 1)

  (let [n-points 5
        point-map (zipmap (get-random-points n-points)
                          (repeatedly n-points random-color))
        points (keys point-map)]
    #_(doseq [[x y] points]
        (q/ellipse x y 10 10))

    (doseq [x (range w)
            y (range h)]
      (let [closest (closest-to points [x y])
            [a b c] (get point-map closest)]
        (q/with-stroke [a b c]
          (q/stroke-weight 10)
          ;; (q/set-pixel x y (q/color a b c))
          (apply q/point [x y]))))))

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
    :mouse-clicked (d/save-on-click-handler "041025")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
