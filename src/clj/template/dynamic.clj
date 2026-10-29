(ns template.dynamic
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as sh]
   [clojure.string :as str]
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
   (when-let [existing (resolve 'template.dynamic/current-sketch)]
     (when-let [sketch-inst @existing]
       (try
         (when sketch-inst
           (println "Stopping existing sketch...")
           (.exit sketch-inst))
         (catch Exception e
           (println "Error stopping sketch:" (.getMessage e))))))
   (q/defsketch current-sketch
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



(defn- slugify [s]
  (-> (str/lower-case s)
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn- find-applet
  "The sketch defined in `ns` with q/defsketch, else the one started by `sketch`."
  [ns]
  (->> (concat (vals (ns-interns ns)) [(resolve 'template.dynamic/current-sketch)])
       (filter #(and % (bound? %)))
       (map deref)
       (filter #(instance? processing.core.PApplet %))
       first))

(defn publish!
  "Save the current frame of the running sketch as a PNG, with an .edn
  file next to it describing it. The frontend shows every image that has
  an .edn file. Call from the sketch namespace. Stages both files with
  `git add -f` (they are under the ignored images/ dir), but does not commit."
  [title description]
  (let [applet (find-applet *ns*)
        sketch-ns (str *ns*)
        base (str "images/" sketch-ns "/" (slugify title))
        png (io/file (str base ".png"))
        edn (io/file (str base ".edn"))]
    (when-not applet
      (throw (ex-info "No sketch is running." {})))
    (when (or (.exists png) (.exists edn))
      (throw (ex-info (str "Already published: " base) {:base base})))
    (io/make-parents png)
    ;; Copy the frame and save the copy: saving the canvas directly is async.
    (let [^processing.core.PImage frame (.get ^processing.core.PApplet applet)]
      (when-not (.save frame (.getAbsolutePath png))
        (throw (ex-info (str "Could not save " png) {}))))
    (spit edn (format "{:title %s\n :description %s\n :sketch %s\n :published %s}\n"
                      (pr-str title)
                      (pr-str description)
                      sketch-ns
                      (pr-str (str (java.time.LocalDate/now)))))
    (let [{:keys [exit err]} (sh/sh "git" "add" "-f" (str png) (str edn))]
      (when-not (zero? exit)
        (throw (ex-info (str "git add failed: " err) {:base base}))))
    (println "Published and staged" (str png))
    (str png)))

;; Example function to demonstrate usage
(defn print-filename [filename]
  (println "The filename is:" filename))

;; Usage


