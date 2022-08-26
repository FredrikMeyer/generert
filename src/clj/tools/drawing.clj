(ns tools.drawing
  (:require [quil.core :as q]))

(defn save-on-click-handler [name]
  (fn [state event]
    (println "Saved")
    (println state)
    (println name)
    (q/save-frame (str name (hash state) "_" (q/random 0 1) ".tif"))
    state))

(defn redraw [old-state event]
  (when (= (:key event) :r)
    (q/redraw)
    (print "Redrawed."))
  old-state)
