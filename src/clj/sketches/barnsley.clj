(ns sketches.barnsley
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]))

(def w 600)
(def h 600)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  {})

(defn make-transformation [[a b c d e f]]
  (fn [[x y]] [(+ (* a x) (* b y) e) (+ (* c x) (* d y) f)]))

(def f1
  (make-transformation [0 0 0 0.16 0 0]))

(def f2
  (make-transformation [0.85 0.04 -0.04 0.85 0 1.6]))

(def f3
  (make-transformation [0.20 -0.26 0.23 0.22 0 1.6 0.07]))

(def f4
  (make-transformation [-0.15 0.28 0.26 0.24 0 0.44 0.07]))

(defn in-interval [a b x]
  (and (> b x) (< a x)))

(defn transform [p]
  (let [r (rand)]
    (cond
      (< r 0.01) (f1 p)
      (in-interval 0.01 0.86 r) (f2 p)
      (in-interval 0.86 0.93 r) (f3 p)
      (in-interval 0.93 1 r) (f4 p))))

(defn draw []
  (q/stroke 33 70 80 20)
  (q/stroke-weight 1)
  (loop [n 1000000
         p [0 0]]
    (when (> n 0)
      (let [[x y] p
            xm (q/map-range x -2.1820 2.6558 0 w)
            ym (q/map-range y 0 9.9983 h 0)]
        (apply q/point [xm ym]))
      (recur (dec n) (transform p)))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time
   (draw))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore barnsley
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "barnsley")
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

