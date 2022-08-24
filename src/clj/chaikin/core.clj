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

(defn copy-first-to-end
  "Copy the first element of a seq to the end."
  [xs]
  (let [frst (first xs)]
    (concat xs [frst])))

(defn draw-points
  "Given a list of points, draw lines connecting them."
  [pts]
  (loop [ps pts]
    (when (>= (count ps) 2)
      (let [[p1 p2] (take 2 ps)]
        (q/line p1 p2)
        (recur (rest ps))))))

(defn iterate-chaikin
  "Given a list of points, use the Chaikin algorithm to smooth the line between them. If ratio
  is not given, uses default value of 0.75."
  [pts & {:keys [closed ratio] :or {closed false ratio 0.25}}]
  (let [pts-new pts
        segments (partition 2 1 pts-new)
        new-pts (mapcat (fn [[a b]]
                          (let [q (gtl/point-on-line a b ratio)
                                r (gtl/point-on-line a b (- 1 ratio))]
                            [q r])) segments)]
    (if closed
      (copy-first-to-end new-pts)
      new-pts)))

(defn chaikin-curve
  "Generate a Chaikin curve from a list of points. If ratio is not given
   uses default value of 0.25."
  [pts n & {:keys [ratio closed] :or {closed false ratio 0.25}}]
  (nth (iterate (fn [pts] (iterate-chaikin pts :ratio ratio :closed closed)) pts) n))

(defn draw []
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
  (time (draw))
  (println "Done")
  (q/no-loop))

(q/defsketch #_:clj-kondo/ignore chaikin
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked (d/save-on-click-handler "chaikin")
  :key-pressed d/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
