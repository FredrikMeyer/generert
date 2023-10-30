(ns dots_square.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.points :as p]
            [tools.random :as r]))

(def w 900)
(def h 900)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/no-fill)
  {})

(defn average [a b]
  (* 0.5 (+ a b)))

(defn center-square [[a b] [c d]]
  [(average a b) (average c d)])

(defn point-on-circle [center r theta]
  (let [[cx cy] center
        x (+ cx (* r (Math/sin theta)))
        y (+ cy (* r (Math/cos theta)))]
    [x y]))

(defn inside-circle? [center radius pt]
  (< (p/distance-sq pt center) (* radius radius)))

(defn inside-quadrant? [center radius a1 a2 pt]
  (let [[x y] (p/add-pts pt (p/neq center))
        angle (Math/atan2 y x)]
    (and (< angle a2)
         (>= angle a1)
         (<= (Math/sqrt (p/distance-sq center pt)) radius))))

(defn random-lines-in-circle [center radius n]
  (let [[a b] center]
    (doseq [[p1 p2] (partition 2
                               (r/random-pts n
                                             [(- a radius) (+ a radius)]
                                             [(- b radius) (+ b radius)]))]

      (when (and
             (inside-circle? center radius p1)
             (inside-circle? center radius p2))
        (q/line p1 p2)))))

(defn- random-small-lines-in-circle [center radius n]
  (let [[a b] center]
    (doseq [pt (r/random-pts n
                             [(- a radius) (+ a radius)]
                             [(- b radius) (+ b radius)])]

      (let [random-unit-vector (q/random-2d)
            dist-to-center (p/distance-sq center pt)
            scaled (p/add-pts pt (p/mult
                                  (r/random 5 (+ 30 (* 10 0 (Math/sqrt dist-to-center))))
                                  random-unit-vector))]
        (when (and (inside-circle? center radius pt)
                   (inside-circle? center radius scaled)
                   #_(not (or (inside-quadrant? center radius (- (* 0.5 Math/PI) 0.3) (* 0.5 Math/PI) pt)
                              (inside-quadrant? center radius (- (* 0.5 Math/PI) 0.3) (* 0.5 Math/PI) scaled)
                              (inside-quadrant? center radius (- (* 0.5 Math/PI) 1.3) (* 1.5 Math/PI) pt))))
          (q/line pt scaled))))))

(defn draw []

  (let [square-diameter 700
        border (- cx (* 0.5 square-diameter))
        [b1 b2] [[border (+ border square-diameter)] [border (+ border square-diameter)]]
        cq (center-square b1 b2)]

    (q/rect border border square-diameter square-diameter)
    (q/rect (- border 5) (- border 5) (+ 10 square-diameter) (+ 10 square-diameter))

    ;; background oise
    (q/with-stroke [100 20]
      (doseq [_ (range 100000)]
        (let [rand-pt (r/random-pt b1 b2)]
          (apply q/point rand-pt))))

    (q/with-stroke [100 20]
      (random-small-lines-in-circle [cx cy] (- (* 0.5 square-diameter) 100) 10000))
    

    (comment
      (q/with-stroke [100 50 100 80]
        (doseq [pt (r/random-pts 100000 b1 b2)]
          (when (inside-circle? (p/diff-pts cq [100 0]) 100 pt)

            (apply q/point pt))))

      (q/with-stroke [50 50 100 50]
        (doseq [pt (r/random-pts 100000 b1 b2)]
          (when (inside-circle? (p/diff-pts cq [-100 0]) 200 pt)
            (apply q/point pt))))

      (q/with-stroke [80 50 100 50]
        (doseq [pt (r/random-pts 100000 b1 b2)]
          (when (inside-circle? (p/diff-pts cq [-150 0]) 300 pt)
            (apply q/point pt))))))

  #_(q/with-stroke [100 10]
      (random-lines-in-circle [cx cy] 200 10000)))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (draw)
  ;; (time (draw))
  (println "Done")
  (q/no-loop))

(q/defsketch #_:clj-kondo/ignore texture
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked (d/save-on-click-handler "template")
  :key-pressed d/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
