(ns tools.quadtree
  (:require [tools.points :as p]
            [tools.random :as r]
            [clj-async-profiler.core :as prof]))

;; Quadtree
;; bounds region
;; can contain children
;; for now - make one of prescribed depth
;; odd numbered levels corresponds to left-right subdivisions, while
;; even numbered correspond to top-bottom subdivisions

(def empty-tree {:region {:top-left [0 1]
                          :width 1
                          :height 1}
                 :ul nil
                 :ur nil
                 :ll nil
                 :lr nil
                 :points []})

(defn translate-region [region dp]
  (update region :top-left (fn [pt] (p/add-pts pt dp))))

(defn scale-region [region s]
  (-> region
      (update :width #(* % s))
      (update :height #(* % s))))

(defn subdivide-region [region]
  (let [s 0.5
        ul-scaled (scale-region region s)
        new-width (:width ul-scaled)
        new-height (:height ul-scaled)]
    {:ul ul-scaled
     :ur (-> ul-scaled
             (translate-region [new-width 0]))
     :ll (-> ul-scaled
             (translate-region [0 (- new-height)]))
     :lr (-> ul-scaled
             (translate-region [new-width (- new-height)]))}))

(defn inside-region? [region point]
  (let [[a b] (:top-left region)
        w (:width region)
        h (:height region)
        [x y] point]
    (and (<= x (+ a w))
         (>= x a)
         (>= y (- b h))
         (>= b y))))

(defn inside-tree-region? [tree point]
  (let [region (:region tree)]
    (inside-region? region point)))

(defn region-subregion? [region point]
  (let [subdivided (subdivide-region region)]
    (->> (into [] subdivided)
         (filter (fn [[_ v]] (inside-region? v point)))
         (first))))

(defn insert [tree pt]
  (if (inside-tree-region? tree pt)
    (let [curr-points (:points tree)]
      (if (> (count curr-points) 10)
        (let [[new-region-key new-region] (region-subregion? (:region tree) pt)
              subtree (if (nil? (new-region-key tree))
                        (assoc empty-tree :region new-region) (new-region-key tree))]
          (assoc tree new-region-key (insert subtree pt)))
        (update tree :points #(conj % pt))))

    (throw (Exception.
            (str "Point not inside region. Point: " (str pt) " Region: " (str (:region tree)))))))

(defn insert-2 [tree pt]
  (loop [curr-tree tree
         key-list []]
    (if (inside-tree-region? curr-tree pt)
      (let [curr-points (:points curr-tree)]
        (if (> (count curr-points) 10)
          (let [[new-region-key _] (region-subregion? (:region curr-tree) pt)
                subtree (new-region-key curr-tree)]
            (if (nil? subtree)
              (update-in tree (concat key-list [:points]) #(conj % pt))
              (recur subtree (conj key-list new-region-key))))
          (update-in tree (concat key-list [:points]) #(conj % pt))))

      (throw (Exception.
              (str "Point not inside region. Point: " (str pt) " Region: " (str (:region curr-tree))))))))

(defn region->range [region]
  (let [[a b] (:top-left region)
        w (:width region)
        h (:height region)]
    [[a (+ a w)] [(- b h) b]]))

(defn intersects-region? [region range]
  (let [[[a b] [c d]] range
        [[p q] [r s]] (region->range region)]
    (and (< a q) (> b p) (< c s) (> d r))))

(defn inside-range? [range pt]
  (let [[[a b] [c d]] range
        [x y] pt]
    (and (> x a) (< x b) (> y c) (< y d))))

(defn query-range
  "Query for points within range. Range is of the form [[a b] [c d]]."
  [tree range]
  (if (and (not (nil? tree)) (intersects-region? (:region tree) range))
    (concat (filter (fn [pt] (inside-range? range pt)) (:points tree))
            (query-range (:ll tree) range)
            (query-range (:ul tree) range)
            (query-range (:lr tree) range)
            (query-range (:ur tree) range))
    []))

(defn count-tree [tree]
  (loop [queue [tree]
         cnt 0]
    (let [curr-tree (first queue)
          subtrees [(:ll curr-tree) (:lr curr-tree) (:ul curr-tree) (:ur curr-tree)]
          filtered-subtrees (filter (fn [t] (not (nil? t))) subtrees)
          no-pts (count (:points curr-tree))]
      (if (empty? filtered-subtrees)
        (+ cnt no-pts)
        (recur (concat (rest queue) filtered-subtrees) (+ cnt no-pts))))))

(defn nearest-neighbour
  "Return the nearest neighour of `pt` inside `tree`."
  [tree pt]
  (if (inside-tree-region? tree pt)
    ()
    nil))

(defn random-tree [n]
  (loop [pts (r/random-pts n [0.0 1] [0.0 1])
         tree empty-tree]
    (if (empty? pts)
      tree
      (recur (rest pts) (insert-2 tree (first pts))))))

(comment

  (let [tree (random-tree 100000)] (time (count (query-range tree [[0 0.5] [0 1]]))))
  (time (count (filter (fn [p] (inside-range? [[0 0.5] [0 1]] p)) (r/random-pts 100000 [0 1] [0 1]))))
;;;
  )
