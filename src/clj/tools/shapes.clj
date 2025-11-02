(ns tools.shapes
  (:require
   [quil.core :as q]
   [tools.algebra :as alg]
   [tools.points :as pts]
   [tools.points :as p]))

(defprotocol Intersects
  (intersects [this other]))

(defprotocol Drawable
  (draw [this]))

(defprotocol Positioned
  (position [this]))

(defprotocol Movable
  (update-pos [this new-pos]))

(defrecord Circle [center radius]
  Drawable
  (draw [{:keys [center radius]}]
    (let [[x y] center]
      (q/ellipse x y (* 2 radius) (* 2 radius)))))

(defrecord Triangle [a b c]
  Drawable
  (draw [{{x1 :x y1 :y} :a
          {x2 :x y2 :y} :b
          {x3 :x y3 :y} :c}]
    (q/triangle x1 y1 x2 y2 x3 y3))
  Object
  (toString [_]
    (str "Triangle" a b c)))

(defrecord Point [x y]
  Drawable
  (draw [_]
    (q/ellipse x y 2 2))
  Comparable
  (compareTo [_ {a :x b :y}]
    (if-not (= x a)
      (compare x a)
      (compare y b))))

(defn point [x y]
  (->Point x y))

(defn point->tuple [point]
  (let [{x :x y :y} point] [x y]))

(defn triangle
  ([tuples]
   (let [[[ax ay] [bx by] [cx cy]] tuples]
     (triangle (point ax ay)
               (point bx by)
               (point cx cy))))
  ([p1 p2 p3]
   {:pre [(not (alg/is-collinear (p/diff-pts (point->tuple p1)
                                             (point->tuple p2))
                                 (p/diff-pts (point->tuple p2)
                                             (point->tuple p3))))]}
   (->Triangle p1 p2 p3)))

(defn triangle-points [triangle]
  (vals triangle))

;; https://totologic.blogspot.com/2014/01/accurate-point-in-triangle-test.html
(defn barycentric-coordinates [^Triangle triangle ^Point p]
  (let [{{x1 :x y1 :y} :a
         {x2 :x y2 :y} :b
         {x3 :x y3 :y} :c} triangle
        {x :x y :y} p
        d (+ (* (- y2 y3)
                (- x1 x3))
             (* (- x3 x2)
                (- y1 y3)))
        a (/ (+ (* (- y2 y3)
                   (- x x3))
                (* (- x3 x2)
                   (- y y3))) d)
        b (/ (+ (* (- y3 y1)
                   (- x x3))
                (* (- x1 x3)
                   (- y y3))) d)]
    [a b (- 1 (+ a b))]))

(defrecord Rectangle [^double xmin ^double ymin ^double xmax ^double ymax]
  Drawable
  (draw [_]
    (q/rect xmin ymin (- xmax xmin) (- ymax ymin))))

(defrecord LineSegment [^Point p1 ^Point p2]
  Drawable
  (draw [_]
    (let [{x1 :x y1 :y} p1
          {x2 :x y2 :y} p2]
      (q/line x1 y1 x2 y2))))

(defn line-segment
  ([p1 p2]
   (->LineSegment p1 p2))
  ([x1 y1 x2 y2]
   (->LineSegment (point x1 y1) (point x2 y2))))

(defn line-segment-points [line-segment]
  (let [{p1 :p1 p2 :p2} line-segment]
    [p1 p2]))

(defn line-segment-length [this]
  (let [{p1 :p1 p2 :p2} this]
    (Math/sqrt (p/distance-sq (point->tuple p1)
                              (point->tuple p2)))))

(defn triangle-edges [^Triangle triangle]
  (let [{a :a b :b c :c} triangle]
    (set [(line-segment a b)
          (line-segment b c)
          (line-segment c a)])))

(defn pt-intersect-circle [pt circle]
  (let [{x :x y :y} pt
        dist-center (Math/sqrt (pts/distance-sq [x y] (:center circle)))
        dist (- dist-center (:radius circle))]
    (<= dist 0)))

(defn point-intersect-rectangle [pt rect]
  (let [[x y] pt
        {:keys [xmin ymin xmax ymax]} rect]
    (and (>= x xmin)
         (>= y ymin)
         (<= x xmax)
         (<= y ymax))))

(defn closest-edge [{:keys [center]} {:keys [xmin xmax ymin ymax]}]
  (let [[cx cy] center]
    [(cond (< cx xmin) xmin
           (> cx xmax) xmax
           :else cx)
     (cond (< cy ymin) ymin
           (> cy ymax) ymax
           :else cy)]))

(defn rectangle-intersect-rectangle [r1 r2]
  (let [{:keys [xmin ymin xmax ymax]} r1]
    (not (or (> ymin (:ymax r2))
             (< ymax (:ymin r2))
             (> xmin (:xmax r2))
             (< xmax (:xmin r2))))))

(defn rectangle-intersect-circle
  "From here: https://www.jeffreythompson.org/collision-detection/circle-rect.php"
  [rect circle]
  (let [[cx cy] (:center circle)]
    (or (and (< cx (:xmax rect))
             (> cx (:xmin rect))
             (< cy (:ymax rect))
             (> cy (:ymin rect)))
        (let [edge (closest-edge circle rect)]
          (< (pts/distance-sq edge (:center circle)) (Math/pow (:radius circle) 2))))))

(defn circle-intersect-circle [^Circle c1 ^Circle c2]
  (let [dist-centers (Math/sqrt (pts/distance-sq (:center c1)
                                                 (:center c2)))
        [r1 r2] (mapv :radius [c1 c2])]
    (<= dist-centers (+ r1 r2))))

(defn line-intersect-line [l1 l2]
  (let [{{x1 :x y1 :y} :p1
         {x2 :x y2 :y} :p2} l1
        eq1 (alg/points->eqn [x1 y1] [x2 y2])
        eq2 (alg/points->eqn [(get-in l2 [:p1 :x])
                              (get-in l2 [:p1 :y])]
                             [(get-in l2 [:p2 :x])
                              (get-in l2 [:p2 :y])])
        intersection     (alg/intersect-lines eq1 eq2)]
    (when-let [[x y] intersection]
      (if (and (<= (min x1 x2) x)
               (<= x (max x1 x2))
               (<= (min y1 y2) y)
               (<= y (max y1 y2))
               (<= (min (get-in l2 [:p1 :x])
                        (get-in l2 [:p2 :x])) x)
               (<= x (max (get-in l2 [:p1 :x])
                          (get-in l2 [:p2 :x])))
               (<= (min (get-in l2 [:p1 :y])
                        (get-in l2 [:p2 :y])) y)
               (<= y (max (get-in l2 [:p1 :y])
                          (get-in l2 [:p2 :y]))))
        intersection
        nil))))

(defn triangle-intersect-point [^Triangle triangle ^Point p]
  (let [[a b c] (barycentric-coordinates triangle p)]
    (and (>= a 0)
         (>= b 0)
         (>= c 0)
         (<= c 1)
         (<= b 1)
         (<= a 1))))

(extend-protocol Intersects
  Circle
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class (class this))
            (circle-intersect-circle this other)
            (= other-class Rectangle)
            (rectangle-intersect-circle other this)
            (= other-class Point)
            (intersects other this)
            :else (throw (Exception. (str "Ops. Other class: " (class other)))))))

  Rectangle
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class (class this))
            (rectangle-intersect-rectangle this other)
            (= other-class tools.shapes.Circle)
            (rectangle-intersect-circle this other)
            :else (throw (Exception. (str "Ops. Other class: " (class other)))))))

  Point
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class (class this))
            (= this other)
            (= other-class Circle)
            (pt-intersect-circle this other)
            (= other-class Rectangle)
            (point-intersect-rectangle this other)
            (= other-class LineSegment)
            (intersects other this)
            (= other-class Triangle)
            (triangle-intersect-point other this)
            :else (throw (Exception. (str "Ops. Other class: " (class other)))))))

  LineSegment
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class (class this))
            (not (nil? (line-intersect-line this other)))
            (= other-class Point)
            (alg/line-contains-pt
             (let [{x1 :x y1 :y} (:p1 this)
                   {x2 :x y2 :y} (:p2 this)]
               (alg/points->eqn [x1 y1] [x2 y2]))
             [(:x other) (:y other)])
            :else (throw (Exception. (str "Ops. Other class: " (class other)))))))

  Triangle
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class Point)
            (triangle-intersect-point this other)
            :else (throw (Exception. (str "Ops. OTher class: " (class other))))))))

(extend-protocol Positioned
  Circle
  (position [this]
    (:center this))

  Rectangle
  (position [{:keys [xmin xmax ymax ymin]}]
    (let [xmid (* 0.5 (+ xmin xmax))
          ymid (* 0.5 (+ ymin ymax))]
      [xmid ymid]))

  Point
  (position [{:keys [x y]}]
    [x y]))

(extend-protocol Movable
  Circle
  (update-pos [this new-pos]
    (-> this
        (assoc :center new-pos)))

  Rectangle
  (update-pos [{:keys [xmin xmax ymin ymax]} new-pos]
    (let [xmid (* 0.5 (+ xmin xmax))
          ymid (* 0.5 (+ ymin ymax))
          [dx dy] (pts/diff-pts new-pos [xmid ymid])]
      (->Rectangle (+ xmin dx)
                   (+ ymin dy)
                   (+ xmax dx)
                   (+ ymax dy)))))
