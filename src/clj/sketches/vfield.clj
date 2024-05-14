(ns sketches.vfield
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as td]))

(def w 900)
(def h 900)

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/frame-rate 5)
  (q/smooth)
  {})

(defn update-state [state]
  state)

(defn v-field [x y]
  (let [factor 1]
    [(* factor (q/noise (/ x 500) (/ y 500)))
     (* factor (q/noise (+ 510 (/ x 100)) (+ 999 (/ y 100))))]))

(defn iterate-mover-step [mover field]
  (let [amount 10
        {x :x y :y goal :goal} mover
        [x-d y-d] (field x y)
        [x-new y-new] [(+ x x-d) (+ y y-d)]
        x-res (q/constrain x-new 0 w)
        y-res (q/constrain y-new 0 h)]
    {:x x-res
     :y y-res
     :goal goal}))

(defn iterate-mover [mover amount field]
  (loop [cnt amount
         [first & rest :as res] (list mover)]
    (if (= cnt 0) res
        (recur (dec cnt) (conj res (iterate-mover-step first field))))))

(defn get-random-points [n]
  (for [_ (range n)]
    [(q/random 0 (- w 50))
     (q/random 0 (- h 50))]))

(defn point-to-walker [[x y]]
  {:x x :y y :goal [(/ w 2) (/ h 2)]})

(defn draw []
  (q/stroke 100 10)
  (q/stroke-weight 2)
  (q/no-fill)
  (let [start-points (map point-to-walker (get-random-points 10000))
        mapped-points (map (fn [m] (iterate-mover m 50 v-field)) start-points)
        partitioned (map (fn [p] (partition 2 1 p)) mapped-points)]
    (doseq [grp partitioned]
      (doseq [[p1 p2] grp]
        (q/line [(:x p1) (:y p1)] [(:x p2) (:y p2)])
          ;; (q/ellipse (:x p1) (:y p1) 10 10)
        ))))

(defn draw-state [state]
  (println "Drawing")
  (q/background 0)
  (time (draw))
  ;; (draw)
  (q/start-loop)
  (println "Done")
  (q/no-loop))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore attempt
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (td/save-on-click-handler "attempt")
    :key-pressed td/redraw
    :draw draw-state
    :features [:no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
