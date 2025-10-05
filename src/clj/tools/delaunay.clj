(ns tools.delaunay
  (:require
   [tools.shapes :as s]))

;; (set! *warn-on-reflection* true)

(defn circumcircle  ^tools.shapes.Circle [^tools.shapes.Triangle triangle]
  ;; translate [ax ay] -> [0 0]
  (let [{[ax ay] :a
         [bx by] :b
         [cx cy] :c} triangle
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

(defn supertriangle [pts]
  (let [min-x (apply min (map :x pts))
        min-y (apply min (map :y pts))
        max-x (apply max (map :x pts))
        max-y (apply max (map :y pts))
        ]
    (s/->Triangle (s/->Point min-x min-y)
                  (s/->Point min-x (* 2 max-y))
                  (s/->Point (* 2 max-x) min-y))))

(defn triangulate [pts])
