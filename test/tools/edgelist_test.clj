(ns tools.edgelist-test
  (:require
   [clojure.test :refer [deftest is] :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.core :as c]
   [tools.2dtree :as s]
   [tools.points :as p]
   [tools.edgelist :as el]
   [tools.random :as r]))

(deftest edgelist
  (t/testing "return vertices"
    (let [edge-list (el/->DoubleConnectedEdgeList '(el/->Vertex 0 0 {}))]
      (is (= '(el/->Vertex 0 0 {}) (el/vertices edge-list))))))

(deftest halfedge
  (t/testing "add twin"
    (let [before (el/->HalfEdge nil nil nil nil)
          twin (el/->HalfEdge 2 nil nil nil)]
      (el/set-twin! before twin)
      (is (= (el/twin before) twin))))

  (t/testing "twin of twin is same"
    (let [v1 (el/->Vertex 0 0 nil)
          v2 (el/->Vertex 1 1 nil)]
      (el/connect v1 v2)
      (let [edge (el/edge v1)]
        (is (= (el/twin (el/twin edge)) edge))
        (is (= (el/origin edge) v1))
        (is (= (el/origin (el/twin edge)) v2)))))

  (t/testing "add two vertices"
    (let [v1 (el/->Vertex 0 0 nil)
          v2 (el/->Vertex 1 1 nil)
          v3 (el/->Vertex 2 2 nil)]
      (el/connect v1 v2)
      (el/connect v2 v3)
      (is (= v3 (el/origin (el/twin (el/next-edge (el/edge v1))))))
      (is (= v1 (el/origin (el/twin (el/next-edge (el/edge v3))))))))

  (t/testing "add triangle, go in circle"
    (let [v1 (el/->Vertex 0 0 nil)
          v2 (el/->Vertex 1 1 nil)
          v3 (el/->Vertex 2 2 nil)]
      (el/connect v1 v2)
      (el/connect v2 v3)
      (el/connect v3 v1)

      (is (= v1 (->> v1
                     el/edge
                     el/next-edge
                     el/next-edge
                     el/twin
                     el/origin)))))

  (t/testing "triangle with hanging line"
    (let [v1 (el/->Vertex 0 0 nil)
          v2 (el/->Vertex 1 1 nil)
          v3 (el/->Vertex 2 2 nil)
          v4 (el/->Vertex 3 3 nil)]
      (el/connect v1 v2)
      (el/connect v2 v3)
      (el/connect v3 v1)
      (el/connect v3 v4)

      (is (= v1 (->> v1
                     el/edge
                     el/next-edge
                     el/next-edge
                     el/twin
                     el/origin)))

      (is (= v3 (->> v4
                    el/edge
                    el/twin
                    el/origin
                    ))))))
