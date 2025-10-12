(ns tools.delaunay
  (:require
   [tools.shapes :as s]
   [clojure.math :refer [sqrt]]
   [clojure.set :as set]
   [tools.points :as p]))

;; (set! *warn-on-reflection* true)
;; Plan is to implement this https://paulbourke.net/papers/triangulate/

(defn circumcircle
  "Computes the circumcircle of a triangle."
  ^tools.shapes.Circle [^tools.shapes.Triangle triangle]
  {:pre [(instance? tools.shapes.Triangle triangle)]}
  ;; translate [ax ay] -> [0 0]
  (let [{{ax :x ay :y} :a
         {bx :x by :y} :b
         {cx :x cy :y} :c} triangle
        [bx' by'] [(- bx ax) (- by ay)]
        [cx' cy'] [(- cx ax) (- cy ay)]
        d' (* 2 (- (* bx' cy')
                   (* by' cx')))
        ux' (/ (- (* cy' (+ (* bx' bx')
                            (* by' by')))
                  (* by' (+ (* cx' cx')
                            (* cy' cy')))) d')
        uy' (/ (- (* bx' (+ (* cx' cx')
                            (* cy' cy')))
                  (* cx' (+ (* bx' bx')
                            (* by' by')))) d')]
    (s/->Circle [(+ ux' ax)
                 (+ uy' ay)]
                (Math/sqrt (+ (* ux' ux')
                              (* uy' uy'))))))

(defn incircle [^tools.shapes.Triangle triangle]
  ;; Notation from
  ;; https://en.wikipedia.org/wiki/Incircle_and_excircles#Cartesian_coordinates
  (let [{{ax :x ay :y} :a
         {bx :x by :y} :b
         {cx :x  cy :y} :c} triangle
        a2 (p/distance-sq [bx by] [cx cy])
        b2 (p/distance-sq [ax ay] [cx cy])
        c2 (p/distance-sq [ax ay] [bx by])
        a (sqrt a2)
        b (sqrt b2)
        c (sqrt c2)
        d (/ 1 (+ a b c))
        center     (p/mult d (p/add-pts (p/mult a [ax ay])
                                        (p/mult b [bx by])
                                        (p/mult c [cx cy])))
        s (* 0.5 (+ a b c))
        radius (sqrt (/ (* (- s a) (- s b) (- s c)) s))]
    (s/->Circle center radius)))

(defn supertriangle [pts]
  (let [min-x (apply min (map :x pts))
        min-y (apply min (map :y pts))
        max-x (apply max (map :x pts))
        max-y (apply max (map :y pts))]
    (s/triangle (s/->Point min-x min-y)
                (s/->Point min-x (* 2 max-y))
                (s/->Point (* 2 max-x) min-y))))

(defn process-point [triangles pt]
  (reduce (fn [acc curr]
            (let [circum (circumcircle curr)]
              (if (s/intersects circum pt)
                (let [edges (s/triangle-edges curr)]
                  (update acc :edges
                          (fn [old]
                            (set/difference
                             (set/union old edges)
                             (set/intersection old edges)))))
                (update acc :triangles #(cons % curr)))))
          {:triangles []
           :edges (set [])}
          triangles))

(defn new-triangles [edges pt]
  (->> edges
       (map s/line-segment-points)
       (map (fn [[p1 p2]] (s/triangle p1 p2 pt)))))

(defn triangulate [pts]
  (reduce (fn [acc curr]
            (let [{triangles :triangles
                   edges     :edges} (process-point acc curr)
                  new-triangs (new-triangles edges curr)]
              (concat triangles new-triangs)))
          (list (supertriangle pts))
          pts))
