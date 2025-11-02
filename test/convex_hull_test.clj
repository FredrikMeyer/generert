(ns convex-hull-test
  (:require  [clojure.test :as t]
             [tools.points :as p]
             [clojure+.test :as tt]
             [tools.shapes :as s]
             [tools.convex-hull :as c]))

(tt/install!)

(t/deftest convex-hull
  (t/testing "result is subset of input"
    (let [pts [(s/point 0 0) (s/point 1 0) (s/point 0 1) (s/point 1 1) (s/point 0.5 0.5)]
          res (c/convex-hull pts)]
      (t/is (= true (every? #(contains? (set pts) %) res)))))

  (t/testing "left turn"
    (let [pts [(s/point 0 0) (s/point 1 0) (s/point 1 1)]]
      (t/is (= true (apply c/left-turn pts)))
      (t/is (= false (apply c/left-turn (reverse pts)))))))
