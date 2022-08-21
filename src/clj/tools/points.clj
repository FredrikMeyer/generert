(ns tools.points)

(defn add-pts [y x]
  (let [[p1 p2] y
        [q1 q2] x]
    [(+ p1 q1) (+ p2 q2)]))
