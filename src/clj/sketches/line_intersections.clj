(ns sketches.line-intersections
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.chaikin :as c]
   [tools.random :as r]
   [tools.points :as p]
   [tools.shapes :as sh]
   [tools.algebra :as alg]
   [tools.drawing :as d]))

(def w 800)
(def h 800)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/no-fill)
  {})

(defn random-point-circle [r [a b]]
  (let [theta (r/random 0 (* 2 q/PI))
        x (+ a (* r (Math/cos theta)))
        y (+ b (* r (Math/sin theta)))]
    [x y]))

(defn random-line-on-circle []
  (let [[a b] (random-point-circle 200 [cx cy])
        [c d] (random-point-circle (r/random 10 200) [a b])]
    (sh/line-segment a b c d)))

(defn many-nonintersecting-lines [n]
  (loop [c n
         lines []]
    (if (not (zero? c))
      (let [l (random-line-on-circle)]
        (if (some #(sh/intersects % l) lines)
          (recur (dec c) lines)
          (recur (dec c) (conj lines l))))
      lines)))

(defn draw []
  

  (doseq [l (many-nonintersecting-lines 100000)]
    (q/stroke-weight (/ (sh/line-segment-length l) (* 0.05 w)))
    (sh/draw l)))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore mysketch
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "line_intersections")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

