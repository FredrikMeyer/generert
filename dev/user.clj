(ns user
  (:require [quil.core :as q]
            [tools.drawing :as t]
            [sketches.differential-growth :as dgc]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; imports stuff here to improve start time
;; https://github.com/puredanger/startup-time
;; https://clojure.org/guides/dev_startup_time


;; run this to regenerate
;; clojure -M:test/env -e "(binding [*compile-files* true] (require 'user :reload-all))"

(defn new-sketch
  "Create a new sketch file with standard template.

  Usage:
    (new-sketch \"my-sketch\")
    (new-sketch \"my-sketch\" 700 700)

  Creates a file at src/clj/sketches/<name>.clj with boilerplate code."
  ([name]
   (new-sketch name 900 900))
  ([name width height]
   (let [sketch-name (str/replace name #"_" "-")
         file-name (str/replace sketch-name #"-" "_")
         ns-name (str "sketches." file-name)
         file-path (str "src/clj/sketches/" file-name ".clj")
         template (str "(ns " ns-name "
  (:require [quil.core :as q]
            [template.dynamic :as dyn]
            [tools.drawing :as d]))

(def w " width ")
(def h " height ")

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 20)
  (q/no-fill)
  {})

(defn draw [state]
  ;; Your drawing code here
  (q/background 0)

  ;; Example: draw a circle in the center
  (q/ellipse cx cy 100 100))

(comment
  (reset! dyn/draw-height h)
  (reset! dyn/draw-width w)
  (dyn/sketch setup #'draw)

  ;; To enable looping:
  ;; (reset! dyn/loop? true)

  ;; Click to save, press 'r' to redraw
  )
")]
     (if (.exists (io/file file-path))
       (println (str "File already exists: " file-path))
       (do
         (spit file-path template)
         (println (str "Created new sketch: " file-path))
         (println (str "Namespace: " ns-name))
         (println "\nNext steps:")
         (println "1. Open the file and implement your draw function")
         (println "2. Evaluate the comment block to run the sketch")
         (println "3. Click to save frames, press 'r' to redraw")))
     file-path)))
