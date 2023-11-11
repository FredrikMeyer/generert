(ns tools.points
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

(s/def ::point (s/coll-of number? :count 2))
(s/def ::args-add-pts (s/cat :y ::point :x ::point))
(s/def ::args-mult (s/cat :c number? :p ::point))

(s/fdef add-pts
  :args ::args-add-pts
  :ret ::point)

(defn add-pts [y x]
  (let [[p1 p2] y
        [q1 q2] x]
    [(+ p1 q1) (+ p2 q2)]))

(s/fdef mult
  :args ::args-mult
  :ret ::point)

(defn mult [c [a b]]
  [(* c a) (* c b)])


(s/fdef diff-pts
  :args ::args-add-pts
  :ret ::point)

(defn diff-pts [x y]
  (let [negy (mult -1 y)]
    (add-pts  x negy)))

(defn middle-pt [x y]
  (let [diff (diff-pts y x)
        halfed (mult 0.5 diff)]
    (add-pts x halfed)))

(s/fdef normalize
  :args (s/cat :p ::point)
  :ret ::point)

(defn normalize [[a b]]
  (let [l2 (+ (* a a) (* b b))
        l (Math/sqrt l2)]
    [(/ a l) (/ b l)]))

(defn distance-sq [[a b] [x y]]
  (let [neg (mult -1 [x y])
        [u v] (add-pts [a b] neg)
        l2 (+ (* u u) (* v v))]
    l2))

(s/fdef neg
  :args (s/cat :p ::point)
  :ret ::point)

(defn neq [pt]
  (mult -1 pt))
