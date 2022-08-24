(ns tools.random)

(defn random
  ([]
   (rand))
  ([max]
   (* (rand) max))
  ([min max]
   (+ min (random (- max min)))))

(defn random-pt
  "Generate a random point inside the square bunded by [x0, x1] and [y0, y1]."
  [[x0 x1] [y0 y1]]
  [(random x0 x1)
   (random y0 y1)])

(defn random-pts
  "Generates n random points bounded by [x0, x1] and [y0, y1]. See [[random-pt]]."
  [n [x0 x1] [y0 y1]]

  (for [_ (range n)] (random-pt [x0 x1] [y0 y1])))
