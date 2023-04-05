(ns tools.points
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

(s/def ::point (s/coll-of (s/and number? #(not (Double/isNaN %))) :count 2 :kind vector?))
(s/def ::args-two-pts (s/cat :y ::point :x ::point))
(s/def ::args-mult (s/cat :c number? :p ::point))

(s/fdef add-pts
  :args ::args-two-pts
  :ret ::point)

(defn add-pts [y x]
  (let [[p1 p2] y
        [q1 q2] x]
    [(+ p1 q1) (+ p2 q2)]))

(s/fdef mult
  :args ::args-mult
  :ret ::point)

(defn mult [^double c [a b]]
  [(* c a) (* c b)])


(s/fdef diff-pts
  :args ::args-two-pts
  :ret ::point)

(defn diff-pts [x y]
  (let [negy (mult -1 y)]
    (add-pts x negy)))

(s/fdef normalize
  :args (s/cat :p ::point)
  :ret ::point)

(defn normalize [[a b]]
  (let [l2 (+ (* a a) (* b b))
        l (Math/sqrt l2)]
    [(/ a l) (/ b l)]))


(s/fdef distance-sq
  :args ::args-two-pts
  :ret (s/and number? #(>= % 0)))

(defn distance-sq
  "Returns the squared distance between two points [a,b] and [x,y]."
  [[a b] [x y]]
  (let [neg (mult -1 [x y])
        [u v] (add-pts [a b] neg)
        l2 (+ (* u u) (* v v))]
    l2))

(comment
  (st/check `distance-sq)
  )
