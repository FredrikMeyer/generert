(ns tools.points-test
  [:require [clojure.test :refer :all]
   [tools.points :as pts]])

(deftest add-pts-test
  (testing "Test add points"
    (let [a [2 3]
          b [1 1]
          res (pts/add-pts a b)]
      (is (= res [3 4])))))

(deftest mult-test
  (is (= (pts/mult 2 [3 4]) [6 8])))
