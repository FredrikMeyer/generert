(ns tools.delaunay-test
  (:require
   [clojure.test :refer [deftest is] :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.core :as c]
   [tools.delaunay :as d]
   [tools.shapes :as ss]
   [tools.points :as p]
   [tools.random :as r]))

(deftest point-on-line
  (t/testing "supertriangle contains all points"
    (let [pts (->> (r/random-pts 10 [0 10] [0 10])
                   (map (fn [[a b]] (ss/->Point a b))))
          res (d/supertriangle pts)]

      (is (= true (every? (fn [p] (ss/intersects p res))
                          pts))))))
