(ns tools.delaunay
  (:require
   [tools.shapes :as s]
   [clojure.math :refer [sqrt]]
   [clojure.set :as set]
   [tools.algebra :as alg]
   [tools.points :as p]))

;; (set! *warn-on-reflection* true)
;; Plan is to implement this https://paulbourke.net/papers/triangulate/

;; https://perso.uclouvain.be/jean-francois.remacle/LMECA2170/Chap2.pdf

(defn circumcircle
  "Computes the circumcircle of a triangle."
  ^tools.shapes.Circle [^tools.shapes.Triangle triangle]
  {:pre [(not (nil? triangle))
         (instance? tools.shapes.Triangle triangle)]}
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
  (let [min-x (- (apply min (map :x pts)) 0.5)
        min-y (- (apply min (map :y pts)) 0.5)
        max-x (+ (apply max (map :x pts)) 0.5)
        max-y (+ (apply max (map :y pts)) 0.5)]
    (s/triangle (s/->Point min-x min-y)
                (s/->Point min-x (* 2 max-y))
                (s/->Point (* 2 max-x) min-y))))

(defn remove-duplicate-edges [edges]
  (let [res
        (->> edges
             (group-by #(set (s/line-segment-points %)))
             vals
             (filter #(< (count %) 2))
             (map first))]

    res))

(defn is-not-collinear [p1 p2 p3]
  (not
   (alg/is-collinear (s/point->tuple p1)
                     (s/point->tuple p2)
                     (s/point->tuple p3))))

(defn new-triangles [edges pt]
  (->> edges
       (map s/line-segment-points)
       (filter (fn [[p1 p2]] (is-not-collinear p1 p2 pt)))
       (map (fn [[p1 p2]] (s/triangle p1 p2 pt)))))

(defn process-point [triangles pt]
  (-> (reduce (fn [acc curr]
                #_(assert (every? #(instance? tools.shapes.Triangle %)
                                  (:triangles acc)))
                #_(assert (every? #(instance? tools.shapes.LineSegment %)
                                  (:edges acc)))
                (let [circum (circumcircle curr)]
                  (if (s/intersects circum pt)
                    (let [edges (s/triangle-edges curr)]
                      (-> acc
                          (update :edges #(concat edges %))))
                    (update acc :triangles #(cons curr %)))))
              {:triangles []
               :edges     []} triangles)
      (update :edges remove-duplicate-edges)))

(defn same-segment [l1 l2]
  (let [points-1 (s/line-segment-points l1)
        points-2 (s/line-segment-points l2)]
    (= (set points-1) (set points-2))))

(defn triangulate [pts]
  (let [sup (supertriangle pts)
        sup-pts (s/triangle-points sup)]
    (->> (reduce (fn [acc curr-pt]
                   (let [{triangles :triangles
                          edges     :edges} (process-point acc curr-pt)
                         new-triangs (new-triangles edges curr-pt)]

                     (concat triangles new-triangs)))
                 (list sup)
                 (concat pts sup-pts))
         #_((fn [t] (do (doseq [tt t] (println tt)) t)))
         (filter (fn [t] (not
                          (let [pts (s/triangle-points t)]
                            (some #(contains? (set sup-pts) %) pts))))))))

(comment
  (let [pts (map #(apply s/point %) [[15 10]
                                     [30 10]
                                     [0 0]
                                     [20 0]])
        res (triangulate pts)]
    res))

(comment
  (let [pts (map #(apply s/point %) [[2 7]
                                     [6 4]
                                     [6 3]
                                     ])]
    (= 6 (triangulate pts))))
