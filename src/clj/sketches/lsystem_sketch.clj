(ns sketches.lsystem-sketch
  (:require
   [clojure.core.match :as match]
   [quil.core :as q]
   [quil.middleware :as m]
   [sketches.lsystem :as l]
   [tools.drawing :as dr]))

(def w 800)
(def h 800)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 70)
  {})

(def branch-length 10)

(defn draw-line [{length :length width :branch-width opacity :opacity}]
  (q/push-style)
  (q/stroke 100 opacity)
  (q/stroke-weight width)
  (q/line 0 0 0 (- length))
  (q/translate 0 (- length))
  (q/pop-style))

(def angle (q/radians 20.5))

(defn rotate [a noise]
  (q/rotate (q/random (- a noise) (+ a noise))))

(defn char->style [char style]
  (match/match [char]
    [\F] (assoc style :function (fn [] (draw-line style)))
    [\+] (assoc style :function (fn [] (rotate angle (:angle-noise style))))
    [\-] (assoc style :function (fn [] (rotate (- angle) (:angle-noise style))))
    [\[] (-> style
             (assoc :function q/push-matrix)
             (update :branch-width (fn [w] (* 0.4 w)))
             (update :angle-noise (fn [w] (* 1.2 w)))
             (update :opacity (fn [o] (/ o 1.2)))
             (update :length (fn [l] (* 0.7 l))))
    [\]] (-> style
             (assoc :function q/pop-matrix)
             (update :branch-width (fn [w] (/ w 0.4)))
             (update :angle-noise (fn [w] (/ w 1.2)))
             (update :opacity (fn [o] (* o 1.2)))
             (update :length (fn [l] (/ l 0.7))))
    :else (assoc style :function (fn [& _]))))

(defn draw-lsystem [lsystem n]
  (println "Drawing l system")
  (let [string (l/grow lsystem n)]
    (loop [s string
           style {:length branch-length
                  :angle-noise 0.05
                  :branch-width 5
                  :opacity 100
                  :function (fn [& _])}]
      (when (not-empty s)
        (let [new-style (char->style (first s) style)]
          (apply (:function new-style) [])
          (recur (rest s) new-style))))))

(defn draw-ground []
  (q/push-style)
  (q/fill 30 10)
  (q/no-stroke)

  (doseq [hh (range 200 701 10)]
    (q/begin-shape)
    (doseq [x (range 0 w 1)]
      (q/vertex x (+ hh (* 80 (q/noise (+ hh  (/ x 100)))))))
    (q/vertex w h)
    (q/vertex 0 h)
    (q/end-shape :close))

  (q/pop-style))

(defn draw []
  ;; Your drawing here
  (draw-ground)
  ;;
  (comment
    (let [gr (q/create-graphics 800 800)]
      (q/with-graphics gr
        (q/color-mode :hsb 100 100 100 100)
        (q/translate 0 700)
        (q/stroke 100 70)
        (draw-lsystem l/L12 4))
      (q/image gr 0 0)
      (q/image gr 200 20)))

  (comment
    (q/with-translation [100 700]
      (draw-lsystem l/L12 4)))

  (q/with-translation [150 800]
    (q/with-rotation [(/ q/PI 4)]
      (draw-lsystem l/L 4)
      (q/with-rotation [(/ q/PI 3)]
        (draw-lsystem l/L 4)))))

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (time (draw))
  (println "Done.")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore lsystemquil
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (dr/save-on-click-handler "lsystemquil")
    :key-pressed dr/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
