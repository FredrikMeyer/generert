(ns tools.shapes
  (:require
   [tools.points :as pts]
   [quil.core :as q]))

(defprotocol Intersects
  (intersects [this other]))

(defprotocol Drawable
  (draw [this]))

(defrecord Circle [center radius]
  Intersects

  Drawable
  (draw [{:keys [center radius]}]
    (let [[x y] center]
      (q/ellipse x y radius radius))))

(defrecord Point [x y])

(defrecord Rectangle [^double xmin ^double ymin ^double xmax ^double ymax]
  Drawable
  (draw [this]
    (q/rect xmin ymin (- xmax xmin) (- ymax ymin))))

(defmulti intersect-shapes
  (fn [s1 s2]
    (into #{} '(s1 s2))))

(defn pt-intersect-circle [pt circle]
  (let [dist-center (Math/sqrt (pts/distance-sq pt (:center circle)))
        dist (- dist-center (:radius circle))]
    (<= dist 0)))

(defn circle-intersect-circle [c1 c2]
  (let [dist-centers (Math/sqrt (pts/distance-sq (:center c1)
                                                 (:center c2)))
        [r1 r2] (mapv :radius [c1 c2])]
    (<= dist-centers (+ r1 r2))))

(defn point-intersect-rectangle [pt rect]
  (let [[x y] pt
        {:keys [xmin ymin xmax ymax]} rect]
    (and (>= x xmin)
         (>= y ymin)
         (<= x xmax)
         (<= y ymax))))

(defn rectangle-intersect-rectangle [r1 r2]
  (let [{:keys [xmin ymin xmax ymax]} r1]
    (not (or (> ymin (:ymax r2))
             (< ymax (:ymin r2))
             (> xmin (:xmax r2))
             (< xmax (:xmin r2))))))

(defn is-above [{:keys [center]} {:keys [ymax]}]
  (let [[_ y] center]
    (> y ymax)))

(defn is-below [{:keys [center]} {:keys [ymin]}]
  (let [[_ y] center]
    (< y ymin)))

(defn is-left-of [{:keys [center]} {:keys [xmin]}]
  (let [[x _] center]
    (< x xmin)))

(defn is-right-of [{:keys [center]} {:keys [xmax]}]
  (let [[x _] center]
    (> x xmax)))

(defn circle-dist-to-edge [{:keys [center radius]} {:keys [ymin]}])

(defn closest-corner [{:keys [center]} {:keys [xmin xmax ymin ymax]}]
  (let [corners [[xmin ymin] [xmin ymax] [xmax ymin] [xmax ymax]]]
    (->> corners
         (sort-by (fn [p] (pts/distance-sq p center)))
         (first))))

(defn rectangle-intersect-circle [rect circle]
  (let [corner (closest-corner circle rect)]
    (< (pts/distance-sq corner (:center circle)) (:radius circle))))


