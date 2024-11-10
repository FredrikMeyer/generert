(ns sketches.shape-intersect
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.shapes :as shapes]
   [tools.chaikin :as c]
   [tools.drawing :as d]))
(def w 800)
(def h 800)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/frame-rate 20)
  (q/no-fill)
  {:pos [0 0]})

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)

  (q/with-fill [100 100]
    (q/text (str state) 50 50))
  (let [[x y] (:pos state)]
    (if (or (shapes/rectangle-intersect-circle (shapes/->Rectangle cx cy (+ cx 100) (+ cy 100))
                                               (shapes/->Circle [x y] 25))
            (shapes/circle-intersect-circle (shapes/->Circle [x y] 25)
                                            (shapes/->Circle [200 200] 100)))
      (q/with-fill [50 100 100]
        (q/ellipse x y 50 50))
      (q/ellipse x y 50 50)))

  (q/rect cx cy 100 100)
  (q/ellipse 200 200 200 200)

  ;; (q/no-loop)
  )

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore mysketch
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "template")
    :key-pressed d/redraw
    :mouse-moved (fn [old-state {:keys [x y]}] (-> old-state (assoc :pos [x y])))
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
