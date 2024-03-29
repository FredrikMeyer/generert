(ns circle-pack.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as d]
            [tools.random :as r]
            [tools.points :as p]
            [tools.lines :as gtl]
            [tools.2dtree :as twod]
            ;; [kdtree :as kd]
            ))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
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

(defn circles-overlap? [^Circle c1 ^Circle c2]
  (let [center-dist (p/distance-sq (:center c1) (:center c2))
        r1 (:radius c1)
        r2 (:radius c2)]
    (< (Math/sqrt center-dist) (+ r1 r2))))

(defn line-intersects-circle?
  "Line segment defined by pt and angle."
  [line circle]
  (if (not (pt-outside-circle? circle (:point line)))
    true
    (let [c (:center circle)
          r (:radius circle)
          pt (:point line)
          angle (:angle line)
          pc (Math/sqrt (p/distance-sq pt c))
          ;; angle-diff (- angle (Math/atan (/ (second c) (first c))))
          max-angle (if (zero? pc) q/TWO-PI (q/asin (/ r pc)))]
      (or (< (- q/TWO-PI max-angle)  angle)
          (> max-angle angle)))))

(defn closest-circle [circles [a b :as p]]
  (reduce (fn [acc ^Circle curr]
            (let [curr-dist (distance-to-circle curr p)]
              (if (< (abs curr-dist) (abs (:dist acc)))
                {:dist curr-dist :circle curr}
                acc)))
          {:dist 100000 :circle (first circles)}
          circles))

(defn closest-circle-through-line [circles line]
  (loop [circles circles
         closest-so-far {:dist 100000 :circle (first circles)}]
    (if (empty? circles)
      closest-so-far
      (let [curr-circle (peek circles)
            pt (:point line)]
        (if (line-intersects-circle? line curr-circle)
          (let [curr-dist (distance-to-circle curr-circle pt)]
            (recur (pop circles)
                   (if (< (abs curr-dist) (abs (:dist closest-so-far)))
                     {:dist curr-dist :circle curr-circle}
                     closest-so-far)))
          (recur (pop circles) closest-so-far))))))

(defn random-point-on-circle [^Circle circle]
  (let [[x y] (:center circle)
        radius (:radius circle)
        theta (r/random 0 q/TWO-PI)]
    [(+ x (* radius (Math/sin theta)))
     (+ y (* radius (Math/cos theta)))]))

(defn closest-point-on-circle
  "Closest point on circle"
  [^Circle circle pt]
  (let [dist (distance-to-circle circle pt)
        dist-center (Math/sqrt (p/distance-sq pt (:center circle)))
        frac (/ dist dist-center)]
    (if (zero? dist-center) (random-point-on-circle circle)
        (gtl/point-on-line (:center circle) pt (- 1 frac)))))

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

(defn get-random-pt []
  (random-point-on-circle (Circle. [450 450] 50)))

(defn get-random-line []
  {:point [450 450] :angle (r/random 0 q/TWO-PI)})

(defn get-opposite-pt [pt center]
  (let [neg-pt (tools.points/mult -1 pt)
        diff (tools.points/add-pts center neg-pt)
        dbl-diff (tools.points/mult 2 diff)
        opp (tools.points/add-pts pt dbl-diff)]
    opp))

(defn create-tangential-circle [circles pt]
  (let [closest-to-pt (closest-circle circles pt)
        closest-pt (closest-point-on-circle (:circle closest-to-pt) pt)
        new-center (gtl/point-on-line closest-pt pt (r/random 0 0.5))
        r (Math/sqrt (p/distance-sq new-center closest-pt))
        c (Circle. new-center r)]

    {:circle c :dist (:dist closest-to-pt)}))

(defn create-tangential-circle-2 [circles line]  (let [closest-to-line (closest-circle-through-line circles line)
                                                       pt (:point line)
                                                       closest-pt (closest-point-on-circle (:circle closest-to-line) pt)
                                                       new-center (gtl/point-on-line closest-pt pt (r/random 0 0.1))
                                                       r (Math/sqrt (p/distance-sq new-center closest-pt))
                                                       c (Circle. new-center r)]

                                                   {:circle c :dist (distance-to-circle c closest-pt)}))

(defn compute-packing [points-goal max-iterations]
  (loop [n 0
         circles [;; (Circle. [(/ w 2) (/ h 2)] 50)
                  (Circle. [450 450] -400)
                  (Circle. [(/ w 4) (/ h 4)] 0)                 
                  ]]
    (if (and (< n max-iterations) (< (count circles) points-goal))
      (let [line (get-random-line)
            ;; pt (get-random-pt) ;;  (random-point-on-sides 50) ;; (r/random-pt [50 (- w 50)] [50 (- h 50)])
            {c :circle dist :dist} (create-tangential-circle-2 circles line)
            overlap-circles (some identity (map (fn [ci] (circles-overlap? ci c)) (rest circles)))]

        (if (and (or true (> dist 0)) (or false (not overlap-circles))
                 (< (p/distance-sq (:center c) [(/ w 2) (/ h 2)]) (* 450 450))) ;; -50
          (recur (inc n) (conj circles c))
          (recur (inc n) circles)))
      (do
        (println "Done computing. Made " (count circles) "circles. Iterations: " n)
        circles))))

(defn draw-circle [^Circle circle]
  (doseq [r (range 0 (inc (:radius circle)) 3)]
    (let [[x y] (:center circle)]
      (q/ellipse x y r r))))

(defn draw-circle-2 [^Circle circle]
  (let [[x y] (:center circle)
        r (:radius circle)]
    (q/ellipse x y r r)))

(defn gen-circles [max-tries]
  (loop [tree (conj (twod/two-tree)
                    (with-meta [(/ 450. w) (/ 450. h)] {:r 10}))
         circles #{}
         n 0]

    (if (< n max-tries)
      (let [[x y] (r/random-pt [0 w] [0 h])
            nearby-points (twod/intersect-rect tree (twod/->Rectangle (/ (- x 40) w) (/ (- y 40) h) (/ (+ x 40) w) (/ (+ y 40) h)))
            nearby-circles (map (fn [p] (->Circle (p/mult w p) (:r (meta p)))) nearby-points)
            closest-new-circle (closest-circle nearby-circles [x y])
;;            neigbour (p/mult w (twod/nearest tree [(/ x w) (/ y h)]))
;;            neighbour-r (:r (meta neigbour))
;;            dist (Math/sqrt (p/distance-sq neigbour [(/ x 1) (/ y 1)]))
            ;;r (Math/abs (- dist neighbour-r))
            ]

        (cond (and (:circle closest-new-circle) (> (:dist closest-new-circle) 0)
                   (< (+ 0 (Math/sqrt (p/distance-sq [x y] [450 450])))
                      (+ (* 1 400))))
              (let [dist-to-circle (:dist closest-new-circle)
                    new-r (min 20 (Math/abs dist-to-circle))
                    c (:circle closest-new-circle)
                    [c1 c2] (:center c)]

                #_(q/line x y c1 c2)
                (recur (conj tree (with-meta [(/ x w) (/ y h)] {:r new-r}))
                       (conj circles (with-meta (->Circle [x y] new-r) {:p [c1 c2]}))
                       (inc n)))
              :else
              (recur tree circles (inc n))))

      (do
        (println "Done iterating")
        circles))))

(defn draw []

  (q/stroke 100 100)
  ;; (q/no-stroke)
  (q/fill 100 30)

  (println (q/current-stroke))
  (comment
    (doseq [c (compute-packing 10000 20000)]
      (draw-circle-2 c)))

 ;;; old

  (let [res (gen-circles 5000)]
    (doseq [c res]
      (let [[x y] (:p (meta c))
            [a b] (:center c)]
        ;; Draw either tree or circles
        (q/line x y a b)
        #_(draw-circle c))))



  
  (comment

    (loop [tree (conj (twod/two-tree)
                      (with-meta [(/ 450. w) (/ 450. h)] {:r 10}))
           n 0]

      (if (< n 2000)
        (let [[x y] (r/random-pt [0 w] [0 h])
              nearby-points (twod/intersect-rect tree (twod/->Rectangle (/ (- x 40) w) (/ (- y 40) h) (/ (+ x 40) w) (/ (+ y 40) h)))
              nearby-circles (map (fn [p] (->Circle (p/mult w p) (:r (meta p)))) nearby-points)
              closest-new-circle (closest-circle nearby-circles [x y])
              neigbour (p/mult w (twod/nearest tree [(/ x w) (/ y h)]))
              neighbour-r (:r (meta neigbour))
              dist (Math/sqrt (p/distance-sq neigbour [(/ x 1) (/ y 1)]))
              r (Math/abs (- dist neighbour-r))]

          (comment (if (and neigbour
                            (> (- 100000 dist neighbour-r r) 0)
                            (< (+ r (Math/sqrt (p/distance-sq [x y] [450 450])))
                               (+ (* 1 400))))
                     (do
            ;; (q/line x y (first neigbour) (second neigbour))
            ;(q/ellipse x y r r)
                       (recur (conj tree (with-meta [(/ x w) (/ y h)] {:r 10}))
                              (inc n)))
                     (recur tree
                            (inc n))))

          (cond (and (:circle closest-new-circle) (> (:dist closest-new-circle) 0)
                     (< (+ r (Math/sqrt (p/distance-sq [x y] [450 450])))
                        (+ (* 1 400))))
                (let [dist-to-circle (:dist closest-new-circle)
                      new-r (min 20 (Math/abs dist-to-circle))
                      c (:circle closest-new-circle)
                      [c1 c2] (:center c)]
                ;; (q/ellipse x y new-r new-r)
                  (draw-circle c)

                  #_(q/line x y c1 c2)
                  (recur (conj tree (with-meta [(/ x w) (/ y h)] {:r new-r}))
                         (inc n)))
                :else
                (recur tree (inc n))))

        (println "Done iterating"))))
  (println "done"))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  #_(q/with-fill [0 100]
    (q/rect 0 0 w h))
  (time (draw))
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore circle-pack
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "circle-pack")
    :key-pressed d/redraw
    :draw draw-state
    :features [:no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))


