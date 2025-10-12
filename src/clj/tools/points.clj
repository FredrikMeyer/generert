(ns tools.points
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as st]))

(s/def ::point (s/coll-of (s/and number? #(not (Double/isNaN %))) :count 2 :kind vector?))
(s/def ::args-two-pts (s/cat :y ::point :x ::point))
(s/def ::args-mult (s/cat :c number? :p ::point))

(s/fdef add-pts
  :args ::args-two-pts
  :ret ::point)

(defn add-pts
  ([y x]
   {:pre [(seqable? y)
          (seqable? x)]}
   (let [[p1 p2] y
         [q1 q2] x]
     [(+ p1 q1) (+ p2 q2)]))
  ([y x & rest]
   (reduce (fn [acc curr]
             (add-pts acc curr))
           (add-pts x y) rest)))

(s/fdef mult
  :args ::args-mult
  :ret ::point)

(defn mult [c [a b :as p]]
  {:pre [(number? c)]}
  (with-meta
    [(* c a) (* c b)] (meta p)))

(s/fdef diff-pts
  :args ::args-two-pts
  :ret ::point)

(defn diff-pts [x y]
  (let [negy (mult -1 y)]
    (add-pts x negy)))

(defn middle-pt [x y]
  (let [diff (diff-pts y x)
        halfed (mult 0.5 diff)]
    (add-pts x halfed)))

(s/fdef normalize
  :args (s/cat :p ::point)
  :ret ::point)

(defn normalize [[a b :as p]]
  (let [l2 (+ (* a a) (* b b))
        l (Math/sqrt l2)]
    (with-meta [(/ a l) (/ b l)] (meta p))))

(s/fdef distance-sq
  :args ::args-two-pts
  :ret (s/and number? #(>= % 0)))

(defn distance-sq
  "Returns the squared distance between two points [a,b] and [x,y]."
  ([[a b] [x y]]
   (let [neg (mult -1 [x y])
         [u v] (add-pts [a b] neg)
         l2 (+ (* u u) (* v v))]
     l2))
  ([a b x y]
   (distance-sq [a b] [x y])))

(comment
  (st/check `distance-sq))

(s/fdef neg
  :args (s/cat :p ::point)
  :ret ::point)

(defn neq [pt]
  (mult -1 pt))

(comment
  (st/instrument `add-pts))
