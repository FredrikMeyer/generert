(ns sketches.point-cloud
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.random :as r]
            [tools.points :as p]
            [tools.lines :as gtl]
            [tools.2dtree :as twod]
            ;; [kdtree :as kd]
            ))

(def w 700)
(def h 700)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/ellipse-mode :radius)
  (q/rect-mode :radius)
  {})

(defn draw []

  (q/stroke 100 100)
  ;; (q/no-stroke)
  (let [pts (r/random-pts 100 [100 600] [100 600])
        t (reduce conj (twod/two-tree) (map #(p/mult (/ 1 700) %) pts))
        line-pts (map #(gtl/point-on-line [50 50] [650 650] %) (range 0 1 0.01))]
    (doseq [[x y] line-pts]
      (let [[a b] (p/mult 700 (twod/nearest t (p/mult (/ 1 700) [x y])))]
        (q/ellipse a b 3 3))

      #_(q/line [x y] (p/mult 700 (twod/nearest t (p/mult (/ 1 700) [x y])))))

    (doseq [tt (take 1000 t)]
      (let [[x y] (p/mult 700 tt)]
        (q/ellipse x y 1 1))))
  (println "done"))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  #_(q/with-fill [0 100]
      (q/rect 0 0 w h))
  (time (draw))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore circle-pack
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "circle-pack")
    :key-pressed d/redraw
    :draw draw-state
    :features [:no-bind-output :pause-on-error :keep-on-top]
    :middleware [m/fun-mode m/pause-on-error]))


