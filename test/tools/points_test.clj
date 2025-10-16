(ns tools.points-test
  [:require [clojure.test :refer :all]
   [tools.points :as pts]])

(deftest add-pts-test
  (testing "Test add points"
    (let [a [2 3]
          b [1 1]
          res (pts/add-pts a b)]
      (is (= res [3.0 4.0])))))

(deftest mult-test
  (is (= (pts/mult 2 [3 4]) [6.0 8.0]))
  (testing "keeps metadata of point"
    (is (= {:a 2} (meta (pts/mult 2 (with-meta [1 2] {:a 2})))))))

(deftest normalize
  (is (= [1. 0.] (pts/normalize [2 0])))
  (testing "keeps metadata of point"
    (is (= {:a 2} (meta (pts/normalize (with-meta [2 0] {:a 2})))))))
