(ns circle_pack.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.random :as r]
            [tools.points :as p]
            [tools.lines :as gtl]
            ;; [kdtree :as kd]
            ))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/ellipse-mode :radius)
  (q/rect-mode :radius)
  {})

(defrecord Circle [center ^double radius])

(defn pt-outside-circle? [^Circle circle [^double a ^double b]]
  (let [dist-center (p/distance-sq [a b] (:center circle))]
    (> dist-center (:radius circle))))

(defn distance-to-circle [^Circle circle [^double a ^double b :as p]]
  (let [dist-center (Math/sqrt (p/distance-sq p (:center circle)))
        dist-circle (- dist-center (:radius circle))]
    dist-circle))

(defn closest-circle [circles [a b :as p]]
  (reduce (fn [acc ^Circle curr]
            (let [curr-dist (distance-to-circle curr p)]
              (if (< (abs curr-dist) (abs (:dist acc)))
                {:dist curr-dist :circle curr}
                acc)))
          {:dist 100000 :circle (first circles)}
          circles))

(defn closest-point-on-circle
  "Closest point on circle"
  [^Circle circle pt]
  (let [dist (distance-to-circle circle pt)
        dist-center (Math/sqrt (p/distance-sq pt (:center circle)))
        frac (/ dist dist-center)]
    (gtl/point-on-line (:center circle) pt (- 1 frac))))

;;; strategi
;;; idea: start med sirkel i midten
;;; legg til nye sirkler på utsiden og la disse trekkes mot midten
;;; q1: via animasjon eller statisk?

(defn random-point-on-sides [padding]
  (let [side (rand)]
    (cond (< side 0.25)
          (r/random-pt [padding (- w padding)] [padding padding])
          (< side 0.5)
          (r/random-pt [(- w padding) (- w padding)] [padding (- h padding)])
          (< side 0.75)
          (r/random-pt [padding (- w padding)] [(- h padding) (- h padding)])
          (<= side 1)
          (r/random-pt [padding padding] [padding (- h padding)]))))

(defn random-point-on-circle [^Circle circle]
  (let [[x y] (:center circle)
        radius (:radius circle)
        theta (r/random 0 q/TWO-PI)]
    [(+ x (* radius (Math/sin theta)))
     (+ y (* radius (Math/cos theta)))]))

(defn compute-packing [points-goal max-iterations]
  (loop [n 0
         circles [
                  (Circle. [(/ w 2) (/ h 2)] 50)
                  ;; (Circle. [(/ w 4) (/ h 4)] 0)
                  ;; (Circle. [(/ (* 3 w) 4) (/ (* 3 h) 4)] 0)
                  ]]
    (if (and (< n max-iterations) (< (count circles) points-goal))
      (let [pt (random-point-on-circle (Circle. [450 450] 400)) ;;  (random-point-on-sides 50) ;; (r/random-pt [50 (- w 50)] [50 (- h 50)])
            closest-to-pt (closest-circle circles pt)
            closest-pt (closest-point-on-circle (:circle closest-to-pt) pt)
            new-center (gtl/point-on-line closest-pt pt (r/random 0 0.5))
            ;; r (min (Math/sqrt (p/distance-sq new-center closest-pt)) 50)
            r (Math/sqrt (p/distance-sq new-center closest-pt))
            c (Circle. new-center r)]

        ;; (println closest-to-pt closest-pt new-center r)
        (if (and (> (:dist closest-to-pt) 0)
                 (< (p/distance-sq (:center c) [(/ w 2) (/ h 2)]) (* 450 450))) ;; -50
          (recur (inc n) (conj circles c))
          (recur (inc n) circles)))
      (do
        (println "Done computing. Made " (count circles) "circles. Iterations: " n)
        circles))))

(defn draw-circle [^Circle circle]
  (doseq [r (range 1 (:radius circle) 3)]
    (let [[x y] (:center circle)]
      (q/ellipse x y r r))))

(defn draw-circle-2 [^Circle circle]
  (let [[x y] (:center circle)
        r (:radius circle)]
    (q/ellipse x y r r)
    )
  )

(defn draw []
  (q/stroke 100)
  ;; (q/no-stroke)
  (q/no-fill)
  ;; (q/fill 100 50)


  (doseq [c (compute-packing 1000 3000)]
    (draw-circle-2 c))

;;; old
  (comment
    (q/ellipse (/ w 2) (/ h 2) 800 800)

    (loop [tree (kd/build-tree
                 [(with-meta [450 450] {:r 10})])
           n 0]
      ;; (println tree)
      (when (< n 10000)
        (let [[x y] (r/random-pt [0 w] [0 h])
              neigbour (kd/nearest-neighbor tree [x y])
              neighbour-r (:r (meta neigbour))
              dist (Math/sqrt (:dist-squared neigbour))
              r 10 ;; (r/random 2 dist)
              ]
          ;; (println neigbour dist r neighbour-r)
          ;; (println "fff" (- dist neighbour-r r))
          (if (and neigbour
                   (> (- dist neighbour-r r) 0)
                   (< (+ r (Math/sqrt (p/distance-sq [x y] [450 450])))
                      (+ (* 1 400))))
            (do
              (q/ellipse x y r r)
              (recur (kd/insert tree (with-meta [x y] {:r r}))
                     (inc n)))
            (recur tree
                   (inc n))))))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw))
  (println "Done")
  (q/no-loop))

(q/defsketch #_:clj-kondo/ignore circle_pack
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked (d/save-on-click-handler "chaikin")
  :key-pressed d/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
