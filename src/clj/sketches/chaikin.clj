(ns sketches.chaikin
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.chaikin :refer [chaikin-curve copy-first-to-end]]
   [tools.drawing :as d]
   [tools.random :as r]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 10)
  {})

(defn draw-points
  "Given a list of points, draw lines connecting them."
  [pts]
  (loop [ps pts]
    (when (>= (count ps) 2)
      (let [[p1 p2] (take 2 ps)]
        (q/line p1 p2)
        (recur (rest ps))))))

(defn draw [_]
  (q/stroke 100)

  (comment
    (draw-points
     (chaikin-curve
      (copy-first-to-end [[100 800] [800 800] [450 100]]) 1)))

  (comment
    (draw-points
     (chaikin-curve
      (copy-first-to-end (r/random-pts 5 [0 900] [0 900])) 5)))

  (comment (doseq [x (range 50 850 100)
                   y (range 50 850 50)]
             (let [pts (r/random-pts 5 [x (+ x 100)] [y (+ y 100)])
                   chaikined (chaikin-curve pts 5 :closed true)]
               (draw-points chaikined))))

  (doseq [y (range 50 850 20)]
    (let [xs (range 50 850 20)
          x-pts (mapcat identity
                        (for [x xs]
                          (r/random-pts 10 [x (+ x 15)] [y (+ y 20)])))]
      (->> x-pts
           ((fn [ps] (chaikin-curve ps 2 :closed false :ratio 0.25)))
           draw-points)))

  (comment
    (doseq [x (range 50 850 100)
            y (range 50 850 100)]
      (let [pts (r/random-pts 10 [x (+ x 100)] [y (+ y 100)])
            pts (copy-first-to-end pts)
            chaikined (chaikin-curve pts 5)]
        (draw-points chaikined)))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw nil))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore chaikin
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "chaikin")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
