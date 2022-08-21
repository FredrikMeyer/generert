(ns tools.random)

(defn random
  ([]
   (rand))
  ([max]
   (* (rand) max))
  ([min max]
   (+ min (random (- max min)))))
