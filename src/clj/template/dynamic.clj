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

(def draw-width (atom 700))
(def draw-height (atom 700))
(def update-state-fn (atom update-state))

(def loop? (atom false))

(defmacro call-with-filename [fn-name]
  `(let [filename# (str *ns*)]
     (~fn-name filename#)))


(defn draw-state [draw prev-state]
  (q/background 0)
  (time (draw prev-state))
  (println "Done")
  (when-not @loop?
    (q/no-loop)))


(defn sketch
  "Create a dynamic sketch."
  ([draw]
   (sketch setup draw))
  ([setup draw]
   ;; Stop any existing sketch first
   (when-let [existing (resolve 'chaikin)]
     (when-let [sketch-inst @existing]
       (try
         (when sketch-inst
           (println "Stopping existing sketch...")
           (.exit sketch-inst))
         (catch Exception e
           (println "Error stopping sketch:" (.getMessage e))))))
   (q/defsketch #_:clj-kondo/ignore chaikin
     :title "You spin my circle right round"
     :size [@draw-width @draw-height]
     :setup (if (var? setup) @setup setup)
     :update @update-state-fn
     :mouse-clicked (call-with-filename d/save-on-click-handler)
     :key-pressed d/redraw
     :draw #(draw-state (if (var? draw) @draw draw) %)
     :features [:keep-on-top :no-bind-output :pause-on-error]
     :middleware [m/fun-mode m/pause-on-error])))

(defn run-sketch
  "Helper to run a sketch with all options in one map.

   Usage:
   (run-sketch {:setup setup-fn
                :update update-fn  ; optional, defaults to identity
                :draw draw-fn
                :width 900         ; optional, uses current @draw-width
                :height 900        ; optional, uses current @draw-height
                :loop? true})      ; optional, uses current @loop?"
  [{:keys [setup update draw width height]
    loop-enabled? :loop?
    :or {update identity}}]
  (when width (reset! draw-width width))
  (when height (reset! draw-height height))
  (when (some? loop-enabled?) (reset! loop? loop-enabled?))
  (reset! update-state-fn update)
  (sketch setup draw))



;; Example function to demonstrate usage
(defn print-filename [filename]
  (println "The filename is:" filename))

;; Usage


