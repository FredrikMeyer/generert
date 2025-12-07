(ns tools.algebra
  (:require
   [tools.points :as p]))

;; (set! *unchecked-math* :warn-on-boxed)

(defn points->eqn
  "Given two points, return its equation of the form ax+by+c=0."
  [[^double x1 ^double y1] [^double x2 ^double y2]]
  (let [a (- y2 y1)
        b (- x1 x2)
        c (- (* y1 x2) (* x1 y2))]
    [a b c]))

;; TODO: should handle cases like this <-->   <---> (i.e parallel lines)
(defn intersect-lines
  "Given two equations of the form ax+by+c=0, return the point of intersection."
  [[a b c] [d e f]]
  (let [det (- (* b d) (* a e))]
    (if (zero? det)
      nil
      [(/ (- (* e c) (* b f)) det)
       (/ (- (* a f) (* d c)) det)])))

(defn line-contains-pt [[a b c] [x y]]
  (let [res (+ (* a x) (* b y) c)]
    (zero? res)))

(defn is-collinear
  "Given two vectors, return true if they are collinear."
  ([v1 v2]
   (loop [r1 v1
          r2 v2
          curr-ratio nil]
     (if (= (count r1) 0) true
         (let [a (first r1)
               b (first r2)]
           (if (and (zero? a) (zero? b))
             (recur (rest r1) (rest r2) nil)
             (if (zero? a)
               false
               (let [ratio (/ b a)]
                 (if (nil? curr-ratio)
                   (recur (rest r1) (rest r2) ratio)
                   (if (not= ratio curr-ratio)
                     false
                     (recur (rest r1) (rest r2) curr-ratio))))))))))
  ([p1 p2 p3]
   (is-collinear (p/diff-pts p1 p2)
                 (p/diff-pts p3 p2))))



