(ns tools.shapes-test
  (:require
   [clojure.test :refer [deftest is]]
   [tools.shapes :as s]))

(deftest intersect-circle-test
  (is (= true
         (s/pt-intersect-circle [0 0] (s/->Circle [0 0] 1))))
  (is (= false
         (s/pt-intersect-circle [0 0] (s/->Circle [1 1] 0.5))))
  (is (= true
         (s/pt-intersect-circle [2 2] (s/->Circle [0 0] 3))))
  (is (= true
         (s/pt-intersect-circle [2 2] (s/->Circle [0 0] 5)))))

(deftest circle-intersect-circle-test
  (is (= true (s/circle-intersect-circle (s/->Circle [0 0] 1)
                                         (s/->Circle [1 1] 2))))
  (is (= false (s/circle-intersect-circle (s/->Circle [0 0] 1)
                                          (s/->Circle [4 4] 1)))))

(deftest point-intersect-rectangle-test
  (is (= true (s/point-intersect-rectangle [1 1] (s/->Rectangle 0 0 2 2))))
  (is (= false (s/point-intersect-rectangle [-1 1] (s/->Rectangle 0 0 2 2)))))

(deftest rectangle-intersect-rectangle-test
  (is (= true (s/rectangle-intersect-rectangle (s/->Rectangle 0 0 1 1)
                                               (s/->Rectangle 0.5 0.5 3 3))))
  (is (= false (s/rectangle-intersect-rectangle (s/->Rectangle 0 0 1 1)
                                                (s/->Rectangle 10 1 11 2)))))

(deftest circle-above-rect
  (is (= true (s/is-above (s/->Circle [0 0] 1) (s/->Rectangle -1 -2 1 -1)))))

(deftest circle-below-rect
  (is (= true (s/is-below (s/->Circle [0 0] 1) (s/->Rectangle -1 2 1 1)))))

(deftest rectangle-intersect-circle-test
  (is (= true (s/rectangle-intersect-circle (s/->Rectangle 0 0 1 1)
                                            (s/->Circle [0 0] 1)))))
