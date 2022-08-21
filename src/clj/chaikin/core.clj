(ns chaikin.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.lines :as gtl]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 10)
  {})

(defn add-pts [y x]
  (let [[p1 p2] y
        [q1 q2] x]
    [(+ p1 q1) (+ p2 q2)]))

(defn random-pt
  "Generate a random point inside the square bunded by [x0, x1] and [y0, y1]."
  [[x0 x1] [y0 y1]]
  [(q/random x0 x1) (q/random y0 y1)])

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

(defn iterate-chaikin [pts]
  (loop [ps pts ;; (conj pts (first pts)) ;; add first point to the end
         new []]
    (if (>= (count ps) 2)
      (let [[a b] (take 2 ps)
            q (gtl/point-on-line a b 0.25)
            r (gtl/point-on-line a b 0.75)]
        (recur (rest ps) (conj new q r)))
      new)))

(defn draw []
  (q/stroke 100)

  (doseq [x (range 50 850 40)
          y (range 50 850 40)]
    (let [pts (random-pts 10 [x (+ x 40)] [y (+ y 40)])]
      (->> pts
           iterate-chaikin
           iterate-chaikin
           iterate-chaikin
           ((fn [l] (concat [[x y]] l [[(+ x 40) (+ y 0)]])))
           draw-points))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

(defn save-on-click [state _]
  (println "Saved")
  (println state)
  (q/save-frame (str "two-lines" (hash state) "_" (q/random 0 1) ".tif"))
  state)

(defn redraw [old-state event]
  (when (= (:key event) :r)
    (q/redraw))
  old-state)

(q/defsketch #_:clj-kondo/ignore chaikin
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked save-on-click
  :key-pressed redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
