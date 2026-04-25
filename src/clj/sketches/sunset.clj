(ns sketches.sunset
  (:require [quil.core :as q]
            [template.dynamic :as dyn]
            [tools.random :as r]
            [tools.points :as p]
            [tools.shapes :as s]
            [thi.ng.color.core :as col]
            [thi.ng.color.presets :as colpre]
            [thi.ng.color.presets.categories :as ccats]
            [tools.drawing :as d]))

(def w 800)
(def h 800)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 20)
  (q/no-fill)
  {})

(defn distance-to-center [pt]
  (Math/sqrt (p/distance-sq pt [cx cy])))

(defn random-pt-upper-diagonal []
  (let [x (r/random 0 w)
        y (r/random 0 (- h x))]
    [x y]))

(defn reflect [pt]
  (let [[x y] pt]
    [(- w x) (- h y)]))

(def colors-blue [:medium-turquoise
                  :skyblue
                  ])

(def colors [:dark-orange
             :gold
             :light-yellow])

(def colors-green [:green
                   :olive-drab
                   :light-green
                   :light-seagreen])

(defn random-color [clrs]
  ;; colpre/colors
  (let [{r :h g :s b :v} (colpre/preset-hsv (rand-nth clrs))]
    [(* 100 r) (* g 100) (* b 100)]))

(defn random-color-2 []
  (let [{r :r g :g b :b} (col/as-rgba (col/int24 (rand-nth ccats/cat20c)))]
    [(* 100 r) (* g 100) (* b 100)]))

(defn draw [state]
  ;; Your drawing code here
  (q/background 100)

  ;; Example: draw a circle in the center
  #_(q/with-fill [100]
      (q/ellipse cx cy 730 730))

  (q/with-stroke [0 20]
    (let [circs-blue [] #_(loop [circs []
                            n 0]
                       (if (> n 5000)
                         circs
                         (let [cntr (r/random-pt [5 (- w 5)] [5 (- cy 5)])
                               r    (r/random 5 (max 10 (- 200 (distance-to-center cntr))))
                               c (s/circle cntr r)]
                           (if (and (not-any? #(s/intersects c %) circs)
                                    (not (s/encloses? (s/circle [cx cy] (/ 720 2)) c)))
                             (recur (conj circs c) (inc n))
                             (recur circs (inc n))))))

          circs-orange (loop [circs []
                              n 0]
                         (if (> n 5000)
                           circs
                           (let [cntr (r/random-pt [0 w] [0 cy])
                                 r    (r/random 5 (max 10 (- 200 (distance-to-center cntr))))
                                 c (s/circle cntr r)]
                             (if (and (not-any? #(s/intersects c %) circs)
                                      (s/encloses? (s/circle [cx cy] (/ 720 2)) c))
                               (recur (conj circs c) (inc n))
                               (recur circs (inc n))))))
          circs-green (loop [circs []
                             n 0]
                        (if (> n 5000)
                          circs
                          (let [cntr (r/random-pt [0 w] [cy h])
                                r    (r/random 5 (* 0.2 (- (second cntr) cy)))
                                c (s/circle cntr r)]
                            (if (not-any? #(s/intersects c %) (concat circs circs-orange))
                              (recur (conj circs c) (inc n))
                              (recur circs (inc n))))))]

      (doseq [c circs-blue]
        (q/with-fill (random-color colors-blue)
          (s/draw c)))
      (doseq [c circs-orange]

        (q/with-fill (random-color colors)
          (s/draw c)))
      (doseq [c circs-green]
        ;; (when (s/encloses? (s/circle [cx cy] (/ 720 2)) c))
        (q/with-fill (random-color colors-green)
          (s/draw c))))

    #_(doseq [_ (range 0 100)]
        (let [c (s/circle (s/point (r/random-pt [0 cx] [0 cy]))
                          50)]
          (s/draw c)))

    #_(doseq [_ (range 0 100)]
        (let [l (s/line-segment (s/point (r/random 0 w) h)
                                (s/point w (r/random 0 h)))]
          (s/draw l)))

    #_(doseq [_ (range 0 200)]
        (let [l (s/line-segment (s/point (r/random 200 w) h)
                                (s/point w (r/random 0 h)))]
          (s/draw l)))

    #_(doseq [_ (range 0 500)]
        (let [l (s/line-segment (s/point (r/random 200 w) h)
                                (s/point w (r/random 0 h)))]
          (s/draw l)))))

(comment
  (reset! dyn/draw-height h)
  (reset! dyn/draw-width w)
  (dyn/sketch setup #'draw)

  ;; To enable looping:
  ;; (reset! dyn/loop? true)

  ;; Click to save, press 'r' to redraw
  )
