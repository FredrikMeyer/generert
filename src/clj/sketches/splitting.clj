(ns sketches.splitting
  (:require
   [clojure.set :as set]
   [quil.core :as q]
   [tools.points :as p]
   [tools.random :as r]
   [tools.delaunay :as del]
   [template.dynamic :as dyn]
   [tools.shapes :as s]))

(def w 900)
(def h 900)

(def cx (/ w 2))
(def cy (/ h 2))

(defn map-values [m f]
  (into {} (for [[k v] m] [k (f v)])))

(defn remaining-corner [p1 p2]
  (first (set/difference #{:a :b :c} #{p1 p2})))

(def opposing-edges
  {:a [:b :c]
   :b [:a :c]
   :c [:a :b]})

(defn rand-edge []
  (-> [:a :b :c]
      rand-nth
      opposing-edges))

(defn midpoint
  ([p1 p2]
   (midpoint p1 p2 0.5))
  ([p1 p2 t]
   (let [{a1 :x b1 :y} p1
         {a2 :x b2 :y} p2]
     (s/point (+ (* t a1) (* (- 1 t) a2))
              (+ (* t b1) (* (- 1 t) b2))
              ))))

(defn longest-edge [t]
  (->> [[:a :b]
        [:b :c]
        [:c :a]]
       (reduce (fn [[v prev] [p1 p2]]
                 (let [a (p1 t)
                       b (p2 t)
                       aa (s/point->tuple a)
                       bb (s/point->tuple b)
                       d (p/distance-sq aa bb)]
                   (if (> d v) [d [p1 p2]] [v prev])))
               [0 nil])
       last))

(defn split-triangle-at
  ([triang edge]
   (split-triangle-at triang edge 0.5))
  ([triang edge t]
   (let [[p1 p2] edge
         mp (midpoint (p1 triang) (p2 triang) t)
         p (remaining-corner p1 p2)]
     [(s/triangle (p1 triang) (p triang) mp)
      (s/triangle (p2 triang) (p triang) mp)])))

(def starting-triangs-1 [(s/triangle (s/point 50 50)
                                     (s/point 50 850)
                                     (s/point 850 850))
                         (s/triangle (s/point 50 50)
                                     (s/point 850 50)
                                     (s/point 850 850))])

(def starting-triangs-2 [(s/triangle
                          (s/point cx 50)
                          (s/point 50 850)
                          (s/point 850 850))])

(defn triangle-split [level]
  (loop [n 0
         triangs starting-triangs-1]
    (if (> n level)
      triangs
      (let [random-edge (rand-edge)
            t (r/random 0.1 0.9)
            splitteds (mapcat #(split-triangle-at % (longest-edge %) t) triangs)]

        (recur (inc n)
               splitteds)))))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/no-fill)
  {})

;; https://www.tylerxhobbs.com/words/aesthetically-pleasing-triangle-subdivision

(defn draw [_]
  ;; (q/with-fill [0 0 0])
  #_(s/draw (s/triangle (s/point 50 50)
                        (s/point 50 850)
                        (s/point 850 850)))

  (doseq [t (triangle-split 7)]
    (s/draw t)
    #_(-> t
          del/incircle
          s/draw)))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw state))
  (println "Done")
  (q/no-loop))

(comment
  (reset! dyn/draw-height h)
  (reset! dyn/draw-width w)
  (dyn/sketch #'draw))



