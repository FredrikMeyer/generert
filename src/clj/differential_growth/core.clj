(ns differential-growth.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.points :as p]))

(def w 900)
(def h 900)

(def cx (/ w 2))
(def cy (/ h 2))

(def insert-distance 15)
(def repulsion-distance (* 5 insert-distance))
(def ^{:doc "Spring constant"} k 5)

(defrecord Particle [pos velocity id])

(defn new-particle [pos]
  (->Particle pos [0 0] (random-uuid)))

(def init-particles (for [n (range 14)]
                      (let [angle (* 2 q/PI (/ n 30))
                            radius 50
                            x (+ cx (* radius (Math/cos angle)))
                            y (+ cy (* radius (Math/sin angle)))]
                        (new-particle [x y]))))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 0 20)
  (q/no-fill)
  {:particles init-particles})

(defn dist-particles [p1 p2]
  (let [p1 (:pos p1)
        p2 (:pos p2)]
    (p/distance-sq p1 p2)))

(defn middle-particle [p1 p2]
  (let [p1 (:pos p1)
        p2 (:pos p2)
        middle-pos (p/middle-pt p1 p2)]
    (new-particle middle-pos)))

(defn iterate-particles [particles]
  (let [n (count particles)]
    (->> (for [[p1 p2] (->> particles
                            cycle
                            (partition 2 1)
                            (take n))]
           (if (> (Math/sqrt (dist-particles p1 p2)) insert-distance)
             [p1 (middle-particle p1 p2)]
             [p1]))
         (flatten))))

(defn weighted-distance [p1 p2]
  (let [diff (p/diff-pts p2 p1)
        dist-sq (p/distance-sq p1 p2)
        diff-mult (p/mult (* -1 (/ 1 dist-sq)) diff)]
    diff-mult))

(defn update-particle [particles p]
  (let [weighter (fn [q] (weighted-distance (:pos p) (:pos q)))
        force-repulsive (->> particles
                             (filter (fn [q]
                                       (and
                                        (not= (:id p) (:id q))
                                        (< (Math/sqrt (p/distance-sq (:pos p) (:pos q)))
                                           repulsion-distance))))
                             (map weighter)
                             (reduce p/add-pts [0 0])
                             (p/mult k))
        force-attractive (->> particles
                              cycle
                              (partition 2 1)
                              (take (count particles)))
        acceleration force-repulsive]
    (update p :velocity (fn [old] (p/add-pts old acceleration)))))

(defn update-attractive-force [particles]
  (let [pairs (->> particles
                   cycle
                   (partition 2 1)
                   (take (count particles)))]
    (for [[p1 p2] pairs]
      (let [dist (p/distance-sq (:pos p1) (:pos p2))
            diff (-> (p/diff-pts (:pos p2) (:pos p1))
                     (p/normalize)
                     (p/mult (/ 1 dist)))]))))

(defn update-pos [p]
  (-> p
      (update :pos (fn [old] (p/add-pts old (:velocity p))))
      (update :velocity (fn [old] (p/mult 0.1 old)))))

(defn update-state [state]
  (-> state
      (update :particles (fn [particles]
                           (map (fn [q] (update-particle particles q)) particles)))
      (update :particles (fn [old] (map update-pos old)))
      (update :particles iterate-particles)
      #_(update :particles iterate-particles)))

(defn draw-state [{particles :particles}]
  (q/background 0)
  (q/stroke 100)
  (q/begin-shape)
  (doseq [particle particles]
    (let [[x y] (:pos particle)]
      ;; (q/ellipse x y 5 5)
      (q/vertex x y)
      #_(println (Math/atan2 y x))))

  (q/end-shape :close)
  ;; (q/no-loop)
  )
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
