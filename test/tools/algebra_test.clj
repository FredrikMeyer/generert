(ns tools.algebra-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [tools.algebra :as subject]))

(deftest points->eqn-test
  (is (subject/is-collinear [1 -1 0]
                            (subject/points->eqn [0 0] [1 1])))
  (is (subject/is-collinear [0 -1 1]
                            (subject/points->eqn [1 1] [2 1])))
  (is (subject/is-collinear [1 0 1]
                            (subject/points->eqn [-1 1] [-1 5])))
  (testing "collinear points"
    (let [[p1 p2 p3] [[0 0] [1 1] [2 2]]]
      (is (subject/is-collinear p1 p2 p3)))))

(deftest intersect-lines
  (is (= [1 1]
         (subject/intersect-lines [1 -1 0]
                                  [0 1 -1])))
  (is (= [0 0]
         (subject/intersect-lines [1 0 0]
                                  [0 1 0])))
  (is (= [-7/5 -2/5]
         (subject/intersect-lines [1 -1 1]
                                  [2 3 4])))
  (is (= nil
         (subject/intersect-lines [1 -1 1]
                                  [1 -1 2]))))
