(ns tools.chaikin
  (:require
   [tools.lines :as gtl]))

(defn copy-first-to-end
  "Copy the first element of a seq to the end."
  [[f & _ :as xs]]
  (concat xs [f]))

(defn iterate-chaikin
  "Given a list of points, use the Chaikin algorithm to smooth the line between them. If ratio
  is not given, uses default value of 0.75."
  [pts & {:keys [closed ratio] :or {closed false ratio 0.25}}]
  (let [pts-new pts
        segments (partition 2 1 pts-new)
        new-pts (mapcat (fn [[a b]]
                          (let [q (gtl/point-on-line a b ratio)
                                r (gtl/point-on-line a b (- 1 ratio))]
                            [q r])) segments)]
    (if closed
      (copy-first-to-end new-pts)
      new-pts)))

(defn chaikin-curve
  "Generate a Chaikin curve from a list of points. If ratio is not given
   uses default value of 0.25."
  [pts n & {:keys [ratio closed] :or {closed false ratio 0.25}}]
  (nth (iterate (fn [pts] (iterate-chaikin pts :ratio ratio :closed closed)) pts) n))
