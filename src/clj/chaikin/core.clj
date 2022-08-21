(ns chaikin.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.random :as r]
            [tools.lines :as gtl]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 10)
  {})

(defn random-pt
  "Generate a random point inside the square bunded by [x0, x1] and [y0, y1]."
  [[x0 x1] [y0 y1]]
  [(r/random x0 x1)
   (r/random y0 y1)])

(defn random-pts
  "Generates n random points bounded by [x0, x1] and [y0, y1]. See [[random-pt]]."
  [n [x0 x1] [y0 y1]]

  (for [_ (range n)] (random-pt [x0 x1] [y0 y1])))

(defn draw-points [pts]
  (loop [ps pts]
    (when (>= (count ps) 2)
      (let [[p1 p2] (take 2 ps)]
        (q/line p1 p2)
        (recur (rest ps))))))

(defn iterate-chaikin
  "Given a list of points, use the Chaikin algorithm to smooth the line between them. If ratio
  is not given, uses default value of 0.75."
  ([pts]
   (iterate-chaikin pts 0.25))
  ([pts ratio]
   (loop [ps pts
          new []]
     (if (>= (count ps) 2)
       (let [[a b] (take 2 ps)
             q (gtl/point-on-line a b ratio)
             r (gtl/point-on-line a b (- 1 ratio))]
         ;; (println a b q r)
         (recur (rest ps) (conj new q r)))
       new))))

(defn chaikin-curve
  ([pts n]
   (nth (iterate iterate-chaikin pts) n)
   )
  ([pts n ratio]
   (nth (iterate (fn [pts] (iterate-chaikin pts ratio)) pts) n) ))

(defn copy-first-to-end [xs]
  (let [frst (first xs)]
    (concat xs [frst])))

(defn draw []
  (q/stroke 100)

  (draw-points (chaikin-curve (copy-first-to-end [[100 800] [800 800] [450 100]]) 3
                              )
               )
  (comment 
    (draw-points
     (chaikin-curve 
      (copy-first-to-end (random-pts 5 [0 900] [0 900])) 0
      )))
  (comment 
    (doseq [x (range 50 850 100)
            y (range 50 850 100)]
      (let [pts (random-pts 10 [x (+ x 100)] [y (+ y 100)])
            pts (copy-first-to-end pts)
            chaikined (chaikin-curve pts 5)]
        (draw-points chaikined)
        ))))

partition

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

(q/defsketch #_:clj-kondo/ignore chaikin
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked d/save-on-click-handler
  :key-pressed d/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
