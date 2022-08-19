(ns chaikin.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.lines :as gtl]))

(def w 900)
(def h 900)

(gtl/point-on-line [0 0] [1 1] 0.25)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 10)
  {})

(defn add-pts [y x]
  (let [[p1 p2] y
        [q1 q2] x]
    [(+ p1 q1) (+ p2 q2)]))

(defn random-pt [[x0 x1] [y0 y1]]
  [(q/random x0 x1) (q/random y0 y1)])

(defn random-pts [n [x0 x1] [y0 y1]]
  (for [_ (range n)] (random-pt [x0 x1] [y0 y1]))
  )

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
  ;; (let [l [[450 450] (random-pt [0 w] [0 900]) [50 100]
  ;;          [150 300] (random-pt [0 w] [0 900]) [200 500] [300 100] [350 400]
  ;;          [400 100] (random-pt [0 w] [0 900]) [450 450]]]
  ;;   (->> l
  ;;        iterate-chaikin
  ;;        iterate-chaikin
  ;;        iterate-chaikin
  ;;        iterate-chaikin
  ;;        iterate-chaikin
  ;;        draw-points))

  (doseq [x (range 50 800 140)
          y (range 50 800 140)]
    (let [pts (random-pts 6 [x (+ x 100)] [y (+ y 100)])]
      (->> pts
           iterate-chaikin
           iterate-chaikin
           draw-points))
    )
  )

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

(defn save-on-click [state event]
  (println "Saved")
  (println state)
  (q/save-frame (str "two-lines" (hash state) "_" (q/random 0 1) ".tif"))
  state)

(defn redraw [old-state event]
  (if (= (:key event) :r)
    (q/redraw))
  old-state)

(q/defsketch #_:clj-kondo/ignore two-lines
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked save-on-click
  :key-pressed redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
