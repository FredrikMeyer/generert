(ns tools.lines)

(defn point-on-line [p1 p2 t]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ (* (- 1 t) x1) (* t x2)),
     (+ (* (- 1 t) y1) (* t y2))]))


