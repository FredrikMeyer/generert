(ns marching-squares.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as tools]))

(def w 600)
(def h 600)

(def delta 5)
(def r 1)
(def N (/ w delta))


;; gruppere linjesegmenter
;; gitt: liste av segmenter [a, b]...
;; output: liste av sammenhengskomponenter

;; algoritme: [a,b] er i samme gruppe some [c,d]:
;; hvis eksisterer sekvens [a',b'], ... med b=a' osv, 

(defn create-vals [t]
  (to-array-2d
   (for [x (range delta (+ w delta) delta)]
     (for [y (range delta (+ h delta) delta)]
       (int (Math/round (q/noise (/ x 50) (/ y 50) t)))
       ))))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/fill 100)
  (q/no-stroke)
  {:t 0
   :vals (create-vals 0)})

(defn to-x [i]
  (+ (* i delta)))

(defn to-y [i]
  (+ (* i delta)))

(defn get-state [vals i j]
  (let [a (aget vals i j)
        b (aget vals (inc i) j)
        c (aget vals (inc i) (inc j))
        d (aget vals i (inc j))]
    (+ d (* 2 c) (* 4 b) (* 8 a))))

(defn draw-m-state [v a b c d]
  (case v
    1 (q/line c d)
    2 (q/line b c)
    3 (q/line d b)
    4 (q/line a b)
    5 (do
        (q/line a d)
        (q/line c b))
    6 (q/line a c)
    7 (q/line a d)
    8 (q/line a d)
    9 (q/line a c)
    10 (do (q/line a b)
           (q/line c d))
    11 (q/line a b)
    12 (q/line b d)
    13 (q/line c b)
    14 (q/line c d)
    15 (identity 0)
    identity))

(defn draw-m-state-filled [v a b c d ul ur bl br]
  (case v
    15 (do
         ;; (q/begin-shape)
         ;; (apply q/vertex ul)
         ;; (apply q/vertex ur)
         ;; (apply q/vertex br)
         ;; (apply q/vertex bl)
         ;; (q/end-shape :close)
         )
    1 (q/line c d)
    2 (q/line b c)
    3 (q/line d b)
    4 (q/line a b)
    5 (do
        (q/line a d)
        (q/line c b))
    6 (q/line a c)
    7 (q/line a d)
    8 (q/line a d)
    9 (q/line a c)
    10 (do (q/line a b)
           (q/line c d))
    11 (q/line a b)
    12 (q/line b d)
    13 (q/line c b)
    14 (q/line c d)
    identity))

(defn in-between-loop [vals]
  ;; (q/stroke-weight 1)
  ;; (q/stroke 0)
  (q/fill 100)
  (q/with-stroke [0 100]
    (doseq [i (range 0 (dec (count vals)))
            j (range 0 (dec (count vals)))]
      (let [x (to-x i)
            y (to-y j)
            state (get-state vals i j)
            ul [x y]
            ur [(+ x delta) y]
            bl [x (+ y delta)]
            br [(+ x delta) (+ y delta)]
            a [(+ x (* 0.5 delta)) y]
            b [(+ x delta) (+ y (* 0.5 delta))]
            c [(+ x (* 0.5 delta)) (+ y delta)]
            d [x (+ y (* 0.5 delta))]]
        ;; (q/line a b)
        (draw-m-state-filled state a b c d ul ur bl br)))))

(defn draw [vals]
  ;; Your drawing here
  (comment
    (doseq [i (range 0 (count vals))
            j (range 0 (count vals))]
      (q/with-fill (q/map-range (aget vals i j) 0 1 0 100)
        ;;(map (fn [c] (q/map-range c -1 1 0 100)) (q/random-3d))
        (q/ellipse (to-x i) (to-y j) r r))))

  (q/stroke-weight 1)
  ;; (doseq [t (range 0 10 0.1)])
  ;; (in-between-loop (create-vals 0))
  (in-between-loop vals)


  )

(defn update-state [state]
  (-> state
      (update :t (fn [v] (+ v 0.1)))
      (assoc :vals (create-vals (:t state)))))

(defn draw-state [state]
  (q/background 100)
  (time (draw (:vals state)))
  ;; (println "Done")

  (q/no-loop)

  )

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore marching-squares
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked  (tools/save-on-click-handler "marching_squares")
    :key-pressed tools/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
