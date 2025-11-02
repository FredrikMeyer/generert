(ns tools.convex-hull
  (:require [tools.shapes :as s]))

(defn left-turn [p q r]
  (let [{px :x py :y} p
        {qx :x qy :y} q
        {rx :x ry :y} r]
    (>
     (-
      (+ (* qx ry)
         (* rx py)
         (* px qy))
      (+ (* rx qy)
         (* px ry)
         (* qx py))) 0)))

(defn prune-pts [pts]
  (loop [curr pts]
    (if (< (count curr) 3)
      curr
      (let [[a b c & r] curr]
        (if (left-turn a b c)
          (recur (concat [a c] r))
          curr)))))

(defn sweep [pts]
  (reduce (fn [acc curr]
            (prune-pts (conj acc curr))) (list)
          pts))

(defn convex-hull [pts]
  (let [sorted-pts (sort pts)
        top-sweep (sweep sorted-pts)
        bottom-sweep (sweep (reverse sorted-pts))]
    (concat top-sweep (drop-last 1 (drop 1 bottom-sweep)))))




