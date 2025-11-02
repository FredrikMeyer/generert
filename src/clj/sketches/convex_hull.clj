(ns sketches.convex-hull
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]
   [tools.points :as p]
   [tools.chaikin :as ch]
   [tools.shapes :as s]
   [tools.convex-hull :as c]
   [tools.random :as r]))

(def w 700)
(def h 700)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  {:pts (map #(apply s/point %) (r/random-pts 50 [0 w] [0 h]))})

(defn drop-random-elt [xs]
  (->> xs
       (shuffle)
       (drop 1)))

(defn draw [state]
  (q/stroke 100)
  ;; (q/stroke-weight 1)

  (let [n-points 100
        ;; pts (map #(apply s/point %) (r/random-pts n-points [0 w] [0 h]))
        pts (:pts state)
        conv-hull (c/convex-hull pts)
        edges (->> (ch/copy-first-to-end conv-hull)
                   (partition 2 1))]
    (doseq [p pts]
      (s/draw p))
    (doseq [[{x1 :x y1 :y} {x2 :x y2 :y}] edges]
      (q/line x1 y1 x2 y2))))

(defn update-state [state]
  (->
   state
   (update :pts #(conj % (apply s/point (r/random-pt [0 w] [0 h]))))
   (update :pts #(drop-random-elt %))))

(defn draw-state [state]
  (q/background 0)
  (time (draw state))
  (println "Done")
  #_(q/no-loop))

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
