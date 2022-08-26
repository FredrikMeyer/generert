(ns tools.drawing
  (:require [quil.core :as q]))

(defn save-on-click-handler
  "Returns a function that returns a save-handler for your sketch."
  [name]
  (fn [state _]
    (println "Saved")
    (println state)
    (println name)
    (q/save-frame (str "images/" name (hash state) "_" (q/random 0 1) ".tif"))
    state))

(defn redraw [old-state event]
  (when (= (:key event) :r)
    (q/redraw)
    (print "Redrawed."))
  old-state)
