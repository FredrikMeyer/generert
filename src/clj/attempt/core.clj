(ns attempt.core
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.drawing :as td]
            [attempt.dynamic :as dyn])
  (:gen-class))

(def w dyn/w)
(def h dyn/h)

;; (defn refresh []
;;   (use :reload 'attempt.dynamic)
;;   (.loop attempt))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore attempt
    :title "You spin my circle right round"
    :size [w h]
    :setup dyn/setup
    :update dyn/update-state
    :mouse-clicked (td/save-on-click-handler "attempt")
    :key-pressed td/redraw
    :draw dyn/draw-state
    :features [:no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
