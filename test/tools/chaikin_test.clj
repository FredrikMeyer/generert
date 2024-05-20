(ns tools.chaikin-test
    (:require [clojure.test :refer [deftest is]]
              [tools.chaikin :as c]))

(deftest copy-first-to-end
  (is (= [0 1 2 3 4 0] (c/copy-first-to-end [0 1 2 3 4]))))
