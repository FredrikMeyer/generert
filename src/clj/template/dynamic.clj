(ns template.dynamic
  (:require
   [quil.core :as q]
   [quil.middleware :as m]
   [tools.drawing :as d]))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/ellipse-mode :radius)
  {})


(defn update-state [state]
  state)

(def ^:dynamic *draw-width* 700)
(def ^:dynamic *draw-height* 700)

(def ^:dynamic *loop* false)

(defmacro call-with-filename [fn-name]
  `(let [filename# (str *ns*)]
     (~fn-name filename#)))


(defn draw-state [draw prev-state]
  (q/background 0)
  (time (draw prev-state))
  (println "Done")
  (when-not *loop*
    (q/no-loop)))


(defn sketch
  ([draw]
   (q/defsketch #_:clj-kondo/ignore chaikin
     :title "You spin my circle right round"
     :size [*draw-width* *draw-height*]
     :setup setup
     :update update-state
     :mouse-clicked (call-with-filename d/save-on-click-handler)
     :key-pressed d/redraw
     :draw #(draw-state draw %)
     :features [:keep-on-top :no-bind-output :pause-on-error]
     :middleware [m/fun-mode m/pause-on-error])))



;; Example function to demonstrate usage
(defn print-filename [filename]
  (println "The filename is:" filename))

;; Usage


