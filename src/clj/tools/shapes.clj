(ns tools.shapes
  (:require
   [tools.points :as pts]
   [tools.algebra :as alg]
   [quil.core :as q]))

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

(defrecord Point [x y]
  Drawable
  (draw [_]
    (q/ellipse x y 2 2)))

(defrecord Rectangle [^double xmin ^double ymin ^double xmax ^double ymax]
  Drawable
  (draw [_]
    (q/rect xmin ymin (- xmax xmin) (- ymax ymin))))

(defrecord LineSegment [^double x1 ^double y1 ^double x2 ^double y2]
  Drawable
  (draw [_]
    (q/line x1 y1 x2 y2)))
(defn pt-intersect-circle [pt circle]
  (let [dist-center (Math/sqrt (pts/distance-sq pt (:center circle)))
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

(defn circle-intersect-circle [c1 c2]
  (let [dist-centers (Math/sqrt (pts/distance-sq (:center c1)
                                                 (:center c2)))
        [r1 r2] (mapv :radius [c1 c2])]
    (<= dist-centers (+ r1 r2))))

(defn line-intersect-line [l1 l2]
  (let [{:keys [x1 y1 x2 y2]} l1
        eq1 (alg/points->eqn [x1 y1] [x2 y2])
        eq2 (alg/points->eqn [(:x1 l2) (:y1 l2)] [(:x2 l2) (:y2 l2)])]
    (alg/intersect-lines eq1 eq2)))

(extend-protocol Intersects
  Circle
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class (class this))
            (circle-intersect-circle this other)
            (= other-class Rectangle)
            (rectangle-intersect-circle other this)
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
            :else (throw (Exception. (str "Ops. Other class: " (class other)))))))

  LineSegment
  (intersects [this other]
    (let [other-class (class other)]
      (cond (= other-class (class this))
            (not (nil? (line-intersect-line this other)))
            (= other-class Point)
            (alg/line-contains-pt
             (alg/points->eqn [(:x1 this) (:y1 this)] [(:x2 this) (:y2 this)])
             [(:x other) (:y other)])
            :else (throw (Exception. (str "Ops. Other class: " (class other))))))))

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
