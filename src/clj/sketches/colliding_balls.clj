(ns sketches.colliding-balls
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.shapes :as shapes]
   [tools.points :as p]
   [tools.random :as r]
   [tools.drawing :as td]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 0 100)
  (q/no-stroke)
  (q/frame-rate 60)
  (q/smooth)
  {:objects [#_{:shape (shapes/->Circle [450 450] 20)
                :direction [2 1]
                :color :red
                :path []}
             #_{:shape (shapes/->Circle [200 450] 15)
                :direction [1 2]
                :color :blue
                :path []}
             #_{:shape (shapes/->Circle [600 850] 20)
                :direction [1 2]
                :color :green
                :path []}
             {:shape (shapes/->Circle [700 700] 10)
                :direction [1 2]
                :color :teal
                :path []}
             {:shape ( shapes/update-pos (shapes/->Rectangle 30 50 150 150) [300 300])
              :direction [1 -2]
              :color :yellow
              :path []}
             {:shape ( shapes/update-pos (shapes/->Rectangle 30 50 150 150) [150 150])
              :direction [2 -2]
              :color :red
              :path []}
             #_{:shape (shapes/->Circle [700 350] 10)
                :direction [3 -2]
                :color :ochre
                :path []}
             #_{:shape (shapes/->Circle [590 350] 20)
                :direction [1 -2]
                :color :burgundy
                :path []}
             #_{:shape (shapes/->Circle [150 700] 30)
                :direction [3 -2]
                :color :pink
                :path []}]})

(defn color->value [color]
  (cond (= :red color)
        [0 100 100]
        (= :blue color)
        [30 100 100]
        (= :green color)
        [70 100 100]
        (= :pink color)
        [89 50 41]
        (= :burgundy color)
        [92 92 67]
        (= :ochre color)
        [8 71 86]
        (= :yellow color)
        [10 100 100]
        [= :teal color]
        [50 100 25]))

(defn collides? [object objects]
  {:pre [(some? object)]}
  (some #(shapes/intersects
          (:shape object) (:shape %))
        (filter #(not (= (:shape %) (:shape object))) objects)))

(defn update-pos [obj objects]
  (let [prev-pos (-> obj :shape shapes/position)
        direction (-> obj :direction)
        new-dir (p/mult 5 (p/normalize (p/add-pts
                                         (p/mult 0.5
                                                 (p/diff-pts (p/mult 2 (r/random-pt)) [1 1]))
                                         (if (or
                                              (collides? obj objects)
                                              (> (p/distance-sq [(* 0.5 w) (* 0.5 h)]
                                                                prev-pos) (* 450 450)))
                                           (p/mult -1 direction)
                                           direction))))
        new-pos (p/add-pts prev-pos new-dir)]
    (-> obj
        (update :shape (fn [old] (shapes/update-pos old new-pos)))
        (assoc :direction new-dir))))

(defn update-state [state]
  (-> state
      (update :objects (fn [objs] (map #(update-pos % objs) objs)))
      (update :objects (fn [objs]
                         (map #(update % :path
                                       (fn [old]
                                         (conj old (-> % :shape shapes/position)))) objs)))))

(defn draw-state [state]
  (q/with-fill [0 1]
    (q/rect 0 0 w h))
  ;; (q/debug state)
  (let [objects (:objects state)]
    (doseq [object objects]
      (let [shape (:shape object)
            path (:path object)]
        (q/with-fill (color->value (:color object))
          (shapes/draw shape)
          ;; (q/ellipse x y (* 2 radius) (* 2 radius))
          )

        (comment
          (q/with-stroke (color->value (:color object))
            (q/begin-shape)
            (doseq [[x y] path]
              (q/vertex x y)
            ;; (q/ellipse x y 5 5)
              )

            (q/end-shape)))))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore attempt
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (td/save-on-click-handler "colliding_balls")
    :key-pressed td/redraw
    :draw draw-state
    :features [:no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))

