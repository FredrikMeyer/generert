(ns user
  (:require [quil.core :as q]
            [tools.drawing :as t]
            [differential-growth.core :as dgc]))

;; importts stuff here to improve start time
;; https://github.com/puredanger/startup-time
;; https://clojure.org/guides/dev_startup_time


;; run this to regenerate
;; clojure -M:test/env -e "(binding [*compile-files* true] (require 'user :reload-all))"
