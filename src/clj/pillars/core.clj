(ns pillars.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as td]))

(def w 900)
(def h 900)

(defn -main [& args])

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/frame-rate 5)
  {})

(def colors [20 80 100])

(defn gaussian [mean std x]
  (let [frac (/ 1 (* std (Math/sqrt (* 2 Math/PI))))
        frac2 (/ -1 (* 2 std std))
        diff (Math/pow (- x mean) 2)]
    (* frac (Math/exp (* frac2 diff)))))

;; TODO put this into gen-art-tools
(defn gaussian-window [f start end]
  ;;
  (let [mean (/ (- end start) 2)
        std 1]
    (fn [x] (* (f x) (gaussian mean std x)))))

;; ((gaussian-window (fn [x] x) 0 100) 100)

(defn draw []
  ;; Your drawing here
  ;; pillar-x lines pillar-y
  (q/fill 100 10)
  (q/no-stroke)
  (doseq [j (range 1 500)]
    (q/fill (rand-nth colors) 100 100 10)
    (q/begin-shape)
    (doseq [x (range 50 (inc (- w 50)) (/ (- w 100) 10))]
      (let [y (+ (* h 0.05) (q/random 0 (* h 0.9)))]
        (q/vertex x y)))
    (q/end-shape :close))

  (q/stroke 100 100)
  (doseq [j (range 1 10)]
    (q/fill 100 40)
    (q/begin-shape)
    (q/vertex 50 (/ h 2))
    (let [seed (q/random 0 1)]
      (doseq [x (range 100 (inc (- w 100)) (/ (- w 100) 500))]
        (let [n (q/noise (+ (* 100 seed) (/ (+ x) 100)))
              ;; TODO start at zero in a nice way
              y (- (* n h 0.8) -100)
              hfn (fn [z] y)
              yy (gaussian-window hfn 100 (- w 100))]
          (q/vertex x y)))
      (q/vertex (- w 50) (/ h 2))
      (q/end-shape)))

  (when (System/getenv "CI_RUN")
    (println "Done drawing, exiting")
    (q/exit)
    (System/exit 0)))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  ;; (time (draw))
  (draw)
  (q/start-loop)
  ;; (println "Done")
  (q/no-loop))

(q/defsketch #_:clj-kondo/ignore pillars
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked (td/save-on-click-handler "pillars")
  :key-pressed td/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
