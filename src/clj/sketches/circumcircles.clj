(ns sketches.circumcircles
  (:require
   [quil.core :as q]
   [tools.shapes :as s]
   [tools.delaunay :as del]
   [tools.random :as r]
   [template.dynamic :as dyn]))

(def w 700)
(def h 700)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  (q/ellipse-mode :radius)
  {})

(defn random-triangle [[a b] [c d]]
  (let [[[ax ay] [bx by] [cx cy]] (r/random-pts 3 [a b] [c d])]
    (s/->Triangle (s/point ax ay)
                  (s/point bx by)
                  (s/point cx cy))))

(defn draw [_]
  (doseq [n (range 10 w 70)
          m (range 10 h 70)]
    (let [triangle (random-triangle [n (+ n 50)] [m (+ m 50)])
          circumcircle (del/circumcircle triangle)
          incircle (del/incircle triangle)
          {{x :x y :y} :center r :radius} circumcircle
          {{xx :x yy :y} :center rr :radius} incircle
          {{ax :x ay :y} :a {bx :x by :y} :b {cx :x cy :y} :c} triangle]
      (q/with-fill [0 0 100 50]
        (q/ellipse x y r r))
        (q/with-fill [3 100 100]
        (q/triangle ax ay bx by cx cy))
      (q/with-fill [40 100 100]
        (q/ellipse xx yy rr rr)))))
