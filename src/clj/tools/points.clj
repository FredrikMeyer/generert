(ns tools.points)

(defn add-pts [y x]
  (let [[p1 p2] y
        [q1 q2] x]
    [(+ p1 q1) (+ p2 q2)]))

(defn mult [c [a b]]
  [(* c a) (* c b)])

(defn diff-pts [x y]
  (let [negy (mult -1 y)]
    (add-pts  x negy)))

(defn normalize [[a b]]
  (let [l2 (+ (* a a) (* b b))
        l (Math/sqrt l2)]
    [(/ a l) (/ b l)]))

(defn distance-sq [[a b] [x y]]
  (let [neg (mult -1 [x y])
        [u v] (add-pts [a b] neg)
        l2 (+ (* u u) (* v v))]
    l2))
