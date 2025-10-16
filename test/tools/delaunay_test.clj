(ns tools.delaunay-test
  (:require
   [clojure.test :refer [deftest is] :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.core :as c]
   [tools.delaunay :as d]
   [tools.shapes :as ss]
   [tools.points :as p]
   [tools.random :as r]
   [tools.shapes :as s]
   [tools.delaunay :as del]))

(defn points-in-triang [triang]
  (->> triang
       (map ss/triangle-points)
       flatten
       set))

(deftest unique-edges
  (t/testing "two duplicate edges removes both"
    (let [res (del/remove-duplicate-edges [(ss/line-segment (s/point 0 0)
                                                            (s/point 1 1))
                                           (ss/line-segment (s/point 1 1)
                                                            (s/point 0 0))])]
      (is (empty? res))))

  (t/testing "two duplicate edges removes both"
    (let [res (del/remove-duplicate-edges [(ss/line-segment (s/point 0 0)
                                                            (s/point 1 1))
                                           (ss/line-segment (s/point 2 3)
                                                            (s/point 5 4))
                                           (ss/line-segment (s/point 1 1)
                                                            (s/point 0 0))])]
      (is (= 1 (count res))))))

(deftest delaunay
  (t/testing "supertriangle contains all points"
    (let [pts (->> (r/random-pts 1000 [0 10] [0 10])
                   (map (fn [[a b]] (ss/->Point a b))))
          res (d/supertriangle pts)]
      (is (= true (every? (fn [p] (ss/intersects p res))
                          pts)))))

  (t/testing "example triangulation"
    (let [pts (map #(apply s/point %) [[0 0] [1 0] [0 1] [0.2 0.2] [0.1 0.1]])]
      (is (= 5 (count (del/triangulate pts))))))

  ;; TODO: fiks denne?? Kanskje utvid supertrekanten?
  #_(t/testing "3 punkter skal bli en trekant"
    (let [pts (map #(apply s/point %) [[2 7]
                                       [6 4]
                                       [6 3]
                                       #_[5 6]])]
      (is (= 6 (del/triangulate pts)))))

  (t/testing "first point far to the left"
    (let [pts (map #(apply s/point %) [[15 10]
                                       [30 10]
                                       [0 0]
                                       [20 0]])
          res (del/triangulate pts)]
      (is (= (count pts) (count (points-in-triang res))))
      (is (= 5 res)))))
