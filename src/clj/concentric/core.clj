(ns concentric.core
  (:require [quil.core :as q]
            [quil.sketch :as qs]
            [quil.applet :as qa]
            [quil.middleware :as m]
            [tools.drawing :as t]))

(def w (atom 900))
(def h (atom 900))

(defn cx [] (/ @w 2))
(defn cy [] (/ @h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/no-fill)
  (q/random-seed 42)
  (q/noise-seed 42)
  {
   :noise-start 0
   :loop true
   })

(defn circle-point [theta r]
  (let [X (+ (cx) (* r (Math/cos theta)))
        Y (+ (cy) (* r (Math/sin theta)))]
    [X Y]))

(defn noise-function [t noise-start]
  (- (q/noise (+ noise-start (Math/sin t))) 0.1))

(defn noise-function2 [t noise-start]
  (* (+ 0.5 (Math/sin t)) (q/noise (+ noise-start (Math/sin t)))))

(defn noise-function3 [t noise-start]
  0)

(defn draw-circle-like
  "Draw circle + noise"
  [radius noise-start]
  (q/begin-shape)
  (doseq [t (range 0 q/TWO-PI 0.1)]
    (let [r (* radius (noise-function t noise-start))
          [x y] (circle-point t r)]
      (q/curve-vertex x y)))
  (q/end-shape :close))

(defn draw [noise-start]
  ;; Your drawing here
  (doseq [i (range 100 700 15)]
    (draw-circle-like i noise-start)))

(defn update-state [state]
  (-> state
      (update :noise-start (fn [n] (+ 0.01 n)))))

(defn draw-state [state]
  (q/background 0)
  (reset! w (q/width))
  (reset! h (q/height))

 
  (draw (:noise-start state))
  (if (not (:loop state)) (q/no-loop)
      (q/start-loop))
  ;; (println "Done")
;  (q/no-loop)
  )

(q/defsketch #_:clj-kondo/ignore concentric
  :title "You spin my circle right round"
  :size [@w @h]
  :setup setup
  :update update-state
  :mouse-clicked (t/save-on-click-handler "concentric")
  :key-pressed t/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error :resizable]
  :middleware [m/fun-mode m/pause-on-error])


t/save-on-click-handler
