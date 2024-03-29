(ns tools.2dtree-test
  (:require
   [clojure.test :refer [deftest is] :as t]
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [criterium.core :as c]
   [tools.2dtree :as s]
   [tools.points :as p]
   [tools.random :as r]))

(defn points-on-circle [n]
  (for [i (range n)]
    (let [x (+ 0.5 (* 0.4 (Math/cos (* 2 Math/PI (/ i n)))))
          y (+ 0.5 (* 0.4 (Math/sin (* 2 Math/PI (/ i n)))))]
      [x y])))

(defn nearest-by-sort [pts pt]
  (-> (sorted-set-by (fn [p q] (compare (p/distance-sq p pt) (p/distance-sq q pt))))
      (into pts)
      (first)))

(deftest TwoTree-test
  (is (= nil
         (s/value (s/->TwoTree nil)))
      (= 2
         (s/value (s/->TwoTree 2))))
  (is (= 2 (-> (s/two-tree)
               (conj [0.1 0.5] [0.2 0.6])
               count)))
  (t/testing "behaves as a set"
    (is (= 1 (-> (s/two-tree)
                 (conj [0.1 0.5] [0.1 0.5])
                 (count)))))
  (t/testing "checks contains"
    (is (= true (-> (s/two-tree)
                    (conj [0.1 0.5] [0.2 0.3])
                    (contains? [0.1 0.5]))))
    (is (= false (-> (s/two-tree)
                     (conj [0.1 0.5] [0.2 0.3] [0.4 0.6])
                     (contains? [0.9 0.9])))))
  (t/testing "Closest pt"
    (is (= [0.3 0.4] (-> (s/two-tree)
                         (conj [0.1 0.2]
                               [0.3 0.4])
                         (s/nearest [0.25 0.35]))))
    (is (= [0.3 0.4] (-> (s/two-tree)
                         (conj [0.1 0.2]
                               [0.7 0.8]
                               [0.3 0.4]
                               [0.3 0.9])
                         (s/nearest [0.3 0.4]))))
    (let [pts [[0.5 0.5]
               [0.25 0.25]
               [0.6 0.45]]
          t (-> (reduce conj (s/two-tree) pts)
                (s/nearest [0.3 0.4]))]
      (is (= (nearest-by-sort pts [0.3 0.4]) t)))
    (let [pts (points-on-circle 10)
          nearest-by-sort (-> (sorted-set-by (fn [p q] (compare (p/distance-sq p [0.5 0.1]) (p/distance-sq q [0.5 0.1]))))
                              (into pts)
                              (first))]
      (is (= nearest-by-sort (-> (reduce conj (s/two-tree) pts)
                                 (s/nearest [0.5 0.1])))))

    (let [pts (r/random-pts 100)
          t (reduce conj (s/two-tree) pts)]
      (is (= (nearest-by-sort pts [0.5 0.5])
             (s/nearest t [0.5 0.5]))))))

(deftest Rectangle-test
  (is (= true (s/contains-point? (s/->Rectangle 0 0 1 1) [0 0]))
      (= false (s/contains-point? (s/->Rectangle 0 0 1 1) [1 2])))
  (is (= 0 (s/distance-squared (s/->Rectangle 0 0 1 1) [0.5 0.5]))
      (= 2 (s/distance-squared (s/->Rectangle 0 0 1 1) [2 2]))))

(deftest left-of-test
  (is (thrown? Exception (s/left-of (s/->Rectangle 0 0 1 1) [5 5])))
  (is (= (s/->Rectangle 0 0 0.5 1) (s/left-of (s/->Rectangle 0 0 1 1) [0.5 0.5]))))

(defn ^:private points-gen [min-length max-length]
  (-> {:infinite? false :max 1 :NaN? false :min 0}
      gen/double*
      (gen/vector 2)
      (gen/vector min-length max-length)))

(def prop (prop/for-all [[p & pts] (points-gen 3 50)]
                        (let [t (reduce conj (s/two-tree) pts)
                              correct-answer (nearest-by-sort pts p)
                              correct-dist (p/distance-sq correct-answer p)
                              tree-answ (s/nearest t p)
                              tree-dist (p/distance-sq tree-answ p)]

                          (= correct-dist tree-dist))))

(deftest nearest-generative
  (is (= nil (let [res (tc/quick-check 10000 prop)]
               (when (not (:pass? res))
                 (let [failed (first (:smallest (:shrunk res)))
                       t (reduce conj (s/two-tree) failed)
                       answ (nearest-by-sort failed [0.5 0.5])
                       tree-answ (s/nearest t [0.5 0.5])]
                   {:tree t
                    :failed failed
                    :root (.root t)
                    :tree-answ tree-answ
                    :tree-dist (p/distance-sq [0.5 0.5] tree-answ)
                    :correct-dist (p/distance-sq [0.5 0.5] answ)
                    :answ answ}))))))

(comment
  (with-redefs [c/*sample-count* 10]
    (let [[p & pts] (gen/sample (points-gen 30 30) 1)
          t (reduce conj (s/two-tree) pts)]
      (c/quick-bench (s/nearest t p) :verbose)))
  ;
  )

(comment
  (with-redefs [c/*sample-count* 60]
    (let [point-sets (gen/sample (points-gen 1000 1000) 100)
          trees (map #(reduce conj (s/two-tree) %) point-sets)]
      (c/bench (doseq [t trees]
                 (let [p (r/random-pt)] (s/nearest t p))) :verbose)))
  ;
  )

