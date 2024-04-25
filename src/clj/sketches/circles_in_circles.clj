(ns sketches.circles-in-circles
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def w 1000)
(def h 1000)

(defn setup []
  (q/frame-rate 2)
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/no-fill)
  {:point [600 600]
   :alpha 50
   :angle 0})

(defn line-to-point [pnt1 pnt]
  (q/line pnt1 pnt))

(defn circles-in-circle [[a b] r p angle]
  (doseq [theta (range angle (+ angle (* 2 q/PI)) 0.1)]
    (let [x (+ a (* r (Math/cos theta)))
          y (+ b (* r (Math/sin theta)))]
      ;; (line-to-point [x y] p)
      (q/fill 0)
      (q/ellipse x y 10 10))))

(defn dist-to-center [x y]
  (let [dx (- x (/ w 2))
        dy (- y (/ h 2))
        dx2 (* dx dx)
        dy2 (* dy dy)]
    (+ dx2 dy2)))

(defn draw [state]
  (q/stroke 100 35)
  (q/background 0)
  ;; (circles-in-circle [(/ w 2) (/ h 2)] 100 (:point state) 0)
  ;; (circles-in-circle [(/ w 2) (/ h 2)] 200 (:point state) (:angle state))

  (let [n-circles 20
        c-w (/ w n-circles)
        c-h (/ h n-circles)]
    (doseq [i (range c-w (- w c-w) c-w)
            j (range c-h (- h c-h) c-h)]
      (let [x-center (+ (/ c-w 2) i)
            y-center (+ (/ c-h 2) j)]
        (when (> (q/random 0 1.2) (/ (Math/sqrt (dist-to-center x-center y-center)) (/ w 2)))
          (q/with-translation [x-center y-center]
            ;; (q/ellipse 0 0 c-w c-h)
            (q/with-rotation [(q/random 0 q/TWO-PI)
                              ;; (q/map-range (* 0.1 (+ i j)) 0 (* n-circles n-circles) 0 q/TWO-PI)
                              ]
              (doseq [k (range 2 40 0.2)]
                (let [inner-radius (/ c-w (+ k 0.5))
                      diam (* inner-radius 2)]
                  (q/ellipse (- (* 1 (/ c-w 2)) inner-radius) 0 diam diam)))))))))

;; (q/start-loop)
  (q/no-loop))

(defn update-state [state]
  state)

(defn save-on-click [state _]
  (println "Saved")
  (println "State: " state)
  (q/save-frame (str "lsystemquil" (hash state) "_" (q/random 0 1) ".png"))
  state)

(defn redraw [old-state event]
  (when (= (:key event) :r)
    (q/redraw))
  old-state)

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore example
    :title "Oh so many grey circles"
    :settings #(q/smooth 8)
    :features [:keep-on-top]
    :setup setup
    :draw draw
    :mouse-clicked save-on-click
    :key-pressed redraw
    :update update-state
    :middleware [m/fun-mode]
    :size [w h]))
