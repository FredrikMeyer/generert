(ns tools.lines
  (:require [clojure.spec.alpha :as s]))

(s/fdef point-on-line
  :args (s/cat :p1 :tools.points/point :p2 :tools.points/point
               :t (s/and number? #(< % 1000) #(> % -1000)))
  :ret :tools.points/point)

(defn point-on-line [p1 p2 t]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ (* (- 1 t) x1) (* t x2)),
     (+ (* (- 1 t) y1) (* t y2))]))

(comment
  (require '[clojure.spec.test.alpha :as st])
  (st/check `point-on-line))
