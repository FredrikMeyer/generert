(ns generative.test-points
  [:require
   [clojure.test :refer :all]
   [tools.points :as pts]])

(deftest point-on-line
  (testing "Test point on line function"
    (let [a [2 3]
          b [1 1]
          res (pts/add-pts a b)]
      (is (= res [3 4])))))

