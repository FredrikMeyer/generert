(ns tools.lines
  (:require [clojure.spec.alpha :as s]
            [tools.points :as p]))

(s/fdef point-on-line
  :args (s/cat :p1 :tools.points/point :p2 :tools.points/point
               :t (s/and number? #(< % 1000) #(> % -1000)))
  :ret :tools.points/point)



(defn point-on-line [p1 p2 ^double t]
  (let [[x1 y1] p1
        [x2 y2] p2]
    [(+ (* (- 1 t) x1) (* t x2)),
     (+ (* (- 1 t) y1) (* t y2))]))

(defn dot-product [[x y] [a b]]
  (+ (* x a) (* y b)))

(defn closest-point-on-line
  "Returns the point on the line defined by two points [a b] closest to p."
  [line p]
  (let [[a b] line
        ap (p/diff-pts p a)
        ab (p/diff-pts b a)
        apab (dot-product ap ab)
        abab (dot-product ab ab)
        c (/ apab abab)]
    (p/add-pts a (p/mult c ab))))


(comment
  (require '[clojure.spec.test.alpha :as st])
  (st/check `point-on-line))
