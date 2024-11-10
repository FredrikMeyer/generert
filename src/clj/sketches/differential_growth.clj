(ns sketches.differential-growth
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.points :as p]
            [tools.random :as r]
            [clj-async-profiler.core :as prof]))

(def w 800)
(def h 800)

(def cx (/ w 2))
(def cy (/ h 2))

(def insert-distance 20)
(def repulsion-distance (* 1 insert-distance))
(def ^{:doc "Spring constant"} k 5)

;; (set! *warn-on-reflection* nil)
;; (set! *unchecked-math* false)

(defrecord Particle [pos ^Double velocity id])

(defn new-particle [pos]
  (->Particle pos [0 0] (str (rand))))

(def init-particles
  "Inserts 14 particles in a circle around the origin."
  (for [n (range 14)]
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

(defn iterate-and-insert-particles [particles]
  (let [particle-pairs (->> [(first particles)]
                            (concat particles)
                            (partition 2 1))]
    (loop [acc (transient [])
           rest-p particle-pairs]
      (if (empty? rest-p)
        (persistent! acc)
        (let [[p1 p2] (first rest-p)
              dist-sq (dist-particles p1 p2)]
          (recur
           (if (> dist-sq (Math/pow insert-distance 2))
             (conj! (conj! acc p1) (middle-particle p1 p2))
             (conj! acc p1))
           (rest rest-p)))))))

(defn weighted-distance [p1 p2]
  (let [diff (p/diff-pts p2 p1)
        dist-sq (p/distance-sq p1 p2)
        diff-mult (p/mult (* -1 (/ 1 dist-sq)) diff)]
    diff-mult))

(defn get-attractive-force [particles]
  (let [pairs (partition 2 1 (concat particles [(first particles)]))]
    (->> pairs
         (map (fn [[p1 p2]]
                (let [dist (p/distance-sq (:pos p1) (:pos p2))
                      diff (->> (p/diff-pts (:pos p2) (:pos p1))
                                (p/normalize)
                                (p/mult (* (if (< (Math/sqrt dist) (* insert-distance 0.5)) -1 1)
                                           (/ 1 dist))))]
                  [(:id p1) diff])))
         (into {}))))

(defn update-particle-velocity [particles attractive-force p]
  (let [weighter (fn [q] (weighted-distance (:pos p) (:pos q)))
        force-repulsive (->> particles
                             (filter (fn [q]
                                       (and
                                        (not= (:id p) (:id q))
                                        (< (p/distance-sq (:pos p) (:pos q))
                                           (* repulsion-distance repulsion-distance)))))
                             (map weighter)
                             (reduce p/add-pts [0 0])
                             (p/mult k))
        force-attractive attractive-force
        acceleration (p/add-pts force-repulsive force-attractive)]
    (update p :velocity (fn [old] (p/add-pts old acceleration)))))

(defn update-pos [p]
  (-> p
      (update :pos (fn [old] (p/add-pts old (:velocity p))))
      (update :velocity (fn [old] (p/mult 0.1 old)))))

(defn update-state [state]
  (let [attractive-force-dict (get-attractive-force (:particles state))]
    (update state :particles
            (fn [particles]
              (-> particles
                  ((fn [particles]
                     (map (fn [q]
                            (update-particle-velocity
                             particles
                             (get attractive-force-dict (:id q)) q))
                          particles)))
                  ((fn [old] (map update-pos old)))
                  (iterate-and-insert-particles))))))

(defn draw-state [{particles :particles}]
  (when nil
    (q/background 0)
    (q/stroke 100))
  (q/begin-shape)
  (doseq [particle particles]
    (let [[x y] (:pos particle)]
      ;; (q/ellipse x y 5 5)
      (q/vertex x y)
      #_(println (Math/atan2 y x))))

  (q/end-shape :close)
  ;; (println "current frame rate" (q/current-frame-rate))
  ;; (q/no-loop)
  )

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore texture
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "template")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

(comment
  (require '[clj-async-profiler.core :as prof])

  (prof/serve-ui 8080)

  (do
    (dotimes [_ 100]
      (let [random-particles (for [i (range 1000)]
                               (new-particle [(r/random 0 400) (r/random 0 400)]))]
        (iterate-and-insert-particles random-particles)))
    (prof/profile
     (dotimes [_ 10000]
       (let [random-particles (for [i (range 200)]
                                (new-particle [(r/random 0 400) (r/random 0 400)]))]
         (iterate-and-insert-particles random-particles))))))

(comment
  "
|                                                   :name |     :n |  :sum |   :q1 |  :med |   :q3 |   :sd |  :mad |
|---------------------------------------------------------+--------+-------+-------+-------+-------+-------+-------|
|              #'differential-growth.core/update-particle | 50 000 | 26,3s | 439µs | 491µs | 557µs | 426µs |  59µs |
|                   #'differential-growth.core/update-pos | 50 000 |  20ms |   0µs |   0µs |   0µs |   1µs |   0µs |
|                   #'differential-growth.core/draw-state |    304 | 680ms | 933µs |   2ms |   3ms |   2ms | 928µs |
|         #'differential-growth.core/get-attractive-force |    302 | 327ms | 294µs | 743µs |   2ms | 947µs | 560µs |
|            #'differential-growth.core/weighted-distance | 50 000 |  23ms |   0µs |   0µs |   0µs |   1µs |   0µs |
|                 #'differential-growth.core/update-state |    302 | 74,9s |   4ms |  56ms | 356ms | 361ms |  55ms |
|                 #'differential-growth.core/new-particle |  4 602 |  20ms |   1µs |   1µs |   4µs |   8µs |   0µs |
|                   #'differential-growth.core/->Particle |  4 602 |   2ms |   0µs |   0µs |   0µs |   1µs |   0µs |
|               #'differential-growth.core/dist-particles | 50 000 |  15ms |   0µs |   0µs |   0µs |   1µs |   0µs |
|              #'differential-growth.core/middle-particle |  4 574 |  28ms |   2µs |   2µs |   6µs |   9µs |   1µs |
| #'differential-growth.core/iterate-and-insert-particles |    302 | 74,6s |   4ms |  56ms | 354ms | 360ms |  54ms |
|                        #'differential-growth.core/setup |      2 | 462µs | 215µs | 247µs | 215µs |  16µs |  31µs |
|                       #'differential-growth.core/sketch |      2 |  96ms |  46ms |  50ms |  46ms |   2ms |   4ms |
")
