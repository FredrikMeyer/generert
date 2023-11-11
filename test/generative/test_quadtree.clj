(ns generative.test-quadtree
  [:require
   [clojure.test :refer :all]
   [tools.quadtree :as q]])

(deftest count-tree
  (testing "count correctly"
    (let [t (q/random-tree 100)]
      (is (= (q/count-tree t) 100)))))

(deftest closest-point
  (testing "closest point, only one point"
    (let [t (-> q/empty-tree
                (q/insert-many [[0.24 0.24]]))]
      (is (= (q/closest-point-2 t [0 0]) [0.24 0.24]))))

  (testing "closest point"
    (binding [q/*tree-capacity* 1]
      (let [t (-> q/empty-tree
                  (q/insert-many [[0.24 0.24] [0.26 0.24] [0.24 0.245] [0.01 0.01]]))]
        (is (= [0.24 0.24] (q/closest-point-2 t [0.25 0])))
        ;; (is (= (q/closest-point-2 t )))
        ))))

q/closest-point-2
