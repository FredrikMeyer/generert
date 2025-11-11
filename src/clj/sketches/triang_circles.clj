(ns sketches.triang-circles
  (:require
   [clojure.math :as math]
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.delaunay :as del]
   [tools.drawing :as d]
   [tools.random :as r]
   [tools.shapes :as s]))

(def w 700)
(def h 700)

(def c (s/point (/ w 2) (/ h 2)))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  (q/ellipse-mode :radius)
  {:alpha 0})

(def colors [[13 32 100]
             [30 30 94]
             [60 30 90]])

(defn random-pt-on-circle [radius center]
  (let [phi (r/random 0 q/TWO-PI)
        {a :x b :y} center]
    (s/point (+ a (* radius (Math/sin phi)))
             (+ b (* radius (Math/cos phi))))))

(defn equidistributed-circle-points [n radius center start]
  (let [is (range n)
        {a :x b :y} center]
    (map #(s/point (+ a (* radius (Math/sin (+ start %))))
                   (+ b (* radius (Math/cos (+ start %))))) is)))

(defn gen-points [n]
  (mapcat identity
          (for [i (range 1 n)]
            (repeatedly (math/round (Math/sqrt (inc i)))
                        #(random-pt-on-circle (inc (* i 10)) c)))))

(defn gen-points-2 [n alpha]
  (mapcat identity
          (for [i (range 1 n)]
            (let [sign (dec (* (mod i 2) 2))]
              (equidistributed-circle-points
               (math/round (Math/sqrt (inc i)))
               (inc (* i 10))
               c
               (+ (* sign alpha) (/ i 10)))))))

(defn draw [state]
  (q/background 100)
  (q/stroke 0 100)

  (let [alpha (:alpha state)
        triangs (del/triangulate (gen-points-2 33 alpha))]
    (doseq [t triangs]
      (q/with-fill (rand-nth colors)
        (s/draw t)))))

(defn update-state [state]
  (-> state
      (update :alpha #(+ % 0.01))))

(defn draw-state [state]
  (q/background 0)
  (q/frame-rate 10)
  (time (draw state))
  (println "Done")
  ;; (q/no-loop)
  )

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore chaikin
    :title "delaunay"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "triang_circles")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

