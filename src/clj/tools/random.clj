(ns tools.random
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]
            [tools.points]
            [clojure.spec.gen.alpha :as gen]))

(defn random
  ([]
   (rand))
  ([max]
   (* (rand) max))
  ([min max]
   (+ min (random (- max min)))))

(s/def ::interval (s/and (s/coll-of number? :count 2) (fn [[a b]] (< a b))))

(s/fdef random-pt
  :args (s/coll-of ::interval :count 2)
  :ret :tools.points/point)

(defn random-pt
  "Generate a random point inside the square bounded by [x0, x1] and [y0, y1]."
  ([[x0 x1] [y0 y1]]
   [(random x0 x1)
    (random y0 y1)])
  ([]
   (random-pt [0 1] [0 1])))

(defn random-pts
  "Generates n random points bounded by [x0, x1] and [y0, y1]. See [[random-pt]]."
  ([n [x0 x1] [y0 y1]]
   (for [_ (range n)] (random-pt [x0 x1] [y0 y1])))
  ([n]
   (random-pts n [0 1] [0 1])))

(comment
  (st/check `random-pt))
