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
                                          (s/->Circle [4 4] 1))))
  (is (= false (s/circle-intersect-circle (s/->Circle [200 450] 50)
                                          (s/->Circle [450 450] 50)))))

(deftest point-intersect-rectangle-test
  (is (= true (s/point-intersect-rectangle [1 1] (s/->Rectangle 0 0 2 2))))
  (is (= false (s/point-intersect-rectangle [-1 1] (s/->Rectangle 0 0 2 2)))))

(deftest rectangle-intersect-rectangle-test
  (is (= true (s/rectangle-intersect-rectangle (s/->Rectangle 0 0 1 1)
                                               (s/->Rectangle 0.5 0.5 3 3))))
  (is (= false (s/rectangle-intersect-rectangle (s/->Rectangle 0 0 1 1)
                                                (s/->Rectangle 10 1 11 2)))))

(deftest rectangle-intersect-circle-test
  (is (= true (s/rectangle-intersect-circle (s/->Rectangle 0 0 1 1)
                                            (s/->Circle [0 0] 1))))
  (is (= false (s/rectangle-intersect-circle (s/->Rectangle 2 0 3 1)
                                             (s/->Circle [0 0] 1))))
  (is (= true (s/rectangle-intersect-circle (s/->Rectangle 0.5 0.5 10 10)
                                            (s/->Circle [0 0] 1))))
  (is (= true (s/rectangle-intersect-circle (s/->Rectangle -1 -1 1 1)
                                            (s/->Circle [0 0] 0.5))))
  (is (= true (s/rectangle-intersect-circle (s/->Rectangle -1 -5 1 5)
                                            (s/->Circle [-1 0] 1)))))
