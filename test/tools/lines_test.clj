(ns tools.lines-test
  (:require [clojure.test :refer :all]
            [tools.lines :as t]))


(deftest point-on-line
  (testing "Test point on line function"
    (let [a (t/point-on-line [0 0] [1 0] 0)
          b (t/point-on-line [0 0] [1 0] 1)]
      (is (= a [0. 0.]))
      (is (= b [1. 0.])))))
