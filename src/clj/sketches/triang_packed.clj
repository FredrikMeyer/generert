(ns sketches.triang-packed
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]
   [tools.points :as p]
   [tools.shapes :as s]
   [tools.random :as r]))


(def w 700)
(def h 700)
(def w2 (/ w 2))
(def h2 (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  ;; (q/stroke 100 10)
  {})

;; ide
"
to kryssende kjegler fylt med sirkler

evt en kjegle fylt med sirkler, men med noen kjegler stikkende ut
"
(defn distance-between [line pt]
  )

(defn draw [_]
  (let [t (s/triangle (s/point w2 0)
                      (s/point (- w2 50) h)
                      (s/point (+ w2 50) h))]
    (s/draw t)
    (doseq [p (r/random-pts 2000 [0 w] [0 h])]
      (let [pp (s/point p)
            c (s/->Circle pp 10)]
        (when (s/encloses? t c)
          (q/with-stroke [0 100 100]
            (s/draw c)))))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw []))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore chaikin
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "triang_packed")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))


