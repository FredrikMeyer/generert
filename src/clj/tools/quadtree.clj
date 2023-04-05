(ns tools.quadtree
  (:require [tools.points :as p]
            [tools.random :as r]
            [clojure.core.match :as m]
            [clojure.spec.alpha :as s]
            [clojure.spec.test.alpha :as stest]
            [clojure.spec.gen.alpha :as gen]
            [clojure.data.priority-map :as pm]
            [clj-async-profiler.core :as prof]))

(def ^:dynamic *tree-capacity* 3)
;; Quadtree
;; bounds region
;; can contain children
;; for now - make one of prescribed depth
;; odd numbered levels corresponds to left-right subdivisions, while
;; even numbered correspond to top-bottom subdivisions

(s/def ::top-left :tools.points/point)

(s/def ::width (s/and number? pos?))
(s/def ::height (s/and number? pos?))
(s/def ::region (s/keys :req [::top-left ::width ::height]))

(s/def ::ul (s/nilable (s/and ::tree)))
(s/def ::ur (s/nilable (s/and ::tree)))
(s/def ::ll (s/nilable (s/and ::tree)))
(s/def ::lr (s/nilable (s/and ::tree)))
(s/def ::points (s/coll-of :tools.points/point :gen-max 3))
(s/def ::tree (s/keys :req [::region ::points] :opt [::ul ::ur ::ll ::lr]))

(comment
  (binding [s/*recursion-limit* 1]
    (gen/generate (s/gen ::tree))))

(def default-region {::top-left [0 1]
                     ::width 1
                     ::height 1})

(def empty-tree {::region default-region
                 ::points []})

(s/fdef region-center
  :args (s/cat :region ::region)
  :ret :tools.points/point)

(defn region-center [region]
  (let [top-left (::top-left region)
        w (::width region)
        h (::height region)]
    (p/add-pts top-left [(* 0.5 w) (* -0.5 h)])))

(defn translate-region [region dp]
  (update region ::top-left (fn [pt] (p/add-pts pt dp))))

(defn scale-region [region ^double s]
  (-> region
      (update ::width #(* % s))
      (update ::height #(* % s))))

(defn subdivide-region
  "Takes a region and creates a new region with a child-region at the specified corner.
  (either ::ul, ::ur, ::ll, ::lr)"
  [region corner]
  (let [s 0.5
        ul-scaled (scale-region region s)
        new-width (::width ul-scaled)
        new-height (::height ul-scaled)]
    (cond (= corner ::ul) ul-scaled
          (= corner ::ur) (-> ul-scaled
                              (translate-region [new-width 0]))
          (= corner ::ll) (-> ul-scaled
                              (translate-region [0 (- new-height)]))
          (= corner ::lr) (-> ul-scaled
                              (translate-region [new-width (- new-height)])))))

(defn inside-region? [region point]
  (let [[a b] (::top-left region)
        w (::width region)
        h (::height region)
        [x y] point]
    (and (<= x (+ a w))
         (>= x a)
         (>= y (- b h))
         (>= b y))))

(defn inside-tree-region? [tree point]
  (let [region (::region tree)]
    (inside-region? region point)))

(defn which-subregion? [region point]
  (let [center (region-center region)
        [dx dy] (p/diff-pts center point)]
    (cond (and (>= dx 0) (>= dy 0))
          ::ll
          (and (>= dx 0) (< dy 0))
          ::ul
          (and (< dx 0) (>= dy 0))
          ::lr
          :else ::ur)))

(defn region-subregion?
  "Returns a tuple consisting [::region new-region] where ::region is one of ::ll, ::ul, ::lr, ::ur, and
  new-region is a subdivided region."
  [region point]
  (let [which-region (which-subregion? region point)
        subdivided (subdivide-region region which-region)]
    [which-region subdivided]))

(defn tree-subtrees [tree]
  (let [corners [::ll ::lr ::ul ::ur]
        subtrees-with-corners (map (fn [c] {::corner c ::tree (c tree)}) corners)]
    (filter (fn [t] (some? (::tree t))) subtrees-with-corners)))

(defn insert [tree pt]
  (loop [tree tree
         key-list []]
    (let [curr-node (get-in tree key-list)]
      (if (inside-tree-region? curr-node pt)
        (let [curr-points (::points curr-node)]
          (if (> (count curr-points) *tree-capacity*)
            (let [[new-region-key new-region] (region-subregion? (::region curr-node) pt)
                  new-key-list (conj key-list new-region-key)
                  subtree (or (new-region-key curr-node)
                              (assoc empty-tree ::region new-region))]

              (recur (assoc-in tree new-key-list subtree) new-key-list))
            (update-in tree (concat key-list [::points]) #(conj % pt))))

        (throw (Exception.
                (str "Point not inside region. Point: " (str pt) " Region: " (str (::region curr-node)))))))))

(defn insert-many [tree pts]
  (reduce (fn [acc curr] (insert acc curr)) tree pts))

(defn create-tree [& {:keys [region points] :or {region default-region points []}}]
  (let [tree-with-region (assoc empty-tree ::region region)]
    (insert-many tree-with-region points)))

(defn point->region-path [tree pt]
  (loop [key-path []]
    (let [curr-node (get-in tree key-path)]

      (if (inside-tree-region? curr-node pt)
        (let [new-region-key (which-subregion? (::region curr-node) pt)
              new-key-list (conj key-path new-region-key)]
          (if (nil? (get-in tree new-key-list)) key-path
              (recur new-key-list)))
        (throw (Exception. (str "Point outside region.")))))))

(defn region->range [region]
  (let [[a b] (::top-left region)
        w (::width region)
        h (::height region)]
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
  (if (and (not (nil? tree)) (intersects-region? (::region tree) range))
    (concat (filter (fn [pt] (inside-range? range pt)) (::points tree))
            (query-range (::ll tree) range)
            (query-range (::ul tree) range)
            (query-range (::lr tree) range)
            (query-range (::ur tree) range))
    []))

(defn count-tree
  "Count the number of elements in `tree`."
  [tree]
  (loop [queue [tree]
         cnt 0]
    (let [curr-tree (first queue)
          subtrees (map ::tree (tree-subtrees curr-tree))
          no-pts (count (::points curr-tree))]
      (if (empty? queue)
        (+ cnt no-pts)
        (recur (into (rest queue) subtrees) (+ cnt no-pts))))))

(defn nearest-neighbours
  "Return the nearest neighours of `pt` inside `tree`. If n is not given, a single point is
  returned.

  This is wrong. An algorithm that works is this one
  https://blog.mapbox.com/a-dive-into-spatial-search-algorithms-ebd0c5e39d2a
  "
  ([tree pt n]
   (loop [points-to-consider []
          key-list []]
     (let [curr-node (get-in tree key-list)
           new-region-key (which-subregion? (::region curr-node) pt)
           subtree (new-region-key curr-node)]
       (if (nil? subtree)
         (let [candidates (into points-to-consider (::points curr-node))
               sorted (sort-by (fn [p] (p/distance-sq p pt)) candidates)]
           (take n sorted))
         (recur (into points-to-consider (::points curr-node)) (conj key-list new-region-key))))))
  ([tree pt]
   (first (nearest-neighbours tree pt 1))))

(defn- opposite-direction [dir]
  (m/match dir
    ::up ::down
    ::right ::left
    ::down ::up
    ::left ::right))

(defn- tree-is-leaf [tree]
  (if (nil? tree) false
      (->> [::ul ::ur ::ll ::lr]
           (map (fn [k] (k tree)))
           (filter some?)
           empty?)))

(defn- match-dir [dir position & {:keys [neigbour] :or {neigbour false}}]
  (m/match [dir position]
    [::up ::ll] ::ul
    [::up ::lr] ::ur
    [::up ::ul] (if neigbour ::ll nil)
    [::up ::ur] (if neigbour ::lr nil)
    [::left ::ur] ::ul
    [::left ::lr] ::ll
    [::left ::ul] (if neigbour ::ur nil)
    [::left ::ll] (if neigbour ::lr nil)
    [::right ::ul] ::ur
    [::right ::ll] ::lr
    [::right ::ur] (if neigbour ::ul nil)
    [::right ::lr] (if neigbour ::ll nil)
    [::down ::ul] ::ll
    [::down ::ur] ::lr
    [::down ::ll] (if neigbour ::ul nil)
    [::down ::lr] (if neigbour ::ur nil)
    :else nil))

(def dir->regions {::up [::ul ::ur]
                   ::left [::ul ::ll]
                   ::down [::ll ::lr]
                   ::right [::ur ::lr]})

(defn- neigbouring-regions-greater-or-equal-size
  "
  Algorithm found here: https://geidav.wordpress.com/2017/12/02/advanced-octrees-4-finding-neighbor-nodes/

  Idea: look for nodes in a given direction. For example, if ::up, then if the current node is already on the
  upper side, look in the parent node.

  If there is no 
  "
  [tree region-path dir]
  (if (zero? (count region-path))
    nil
    (let [parent-path (pop region-path)
          self-position (peek region-path)]

      ;; If self is north, then must look further up
      (if-let [neigbour-corner (match-dir dir self-position)]
        (let [answ (conj parent-path neigbour-corner)]
          ;; If immediate neighour (within current node) does not exist,
          ;; look one level up
          (if (nil? (get-in tree answ))
            (neigbouring-regions-greater-or-equal-size tree parent-path dir)
            answ))
        (let [n (neigbouring-regions-greater-or-equal-size tree parent-path dir)]
          (if (or (nil? n) (tree-is-leaf (get-in tree n)))
            n
            (let [opposite-region (conj n (match-dir dir self-position :neigbour true))]
              (if (nil? (get-in tree opposite-region))
                (neigbouring-regions-greater-or-equal-size tree parent-path dir)
                opposite-region))))))))

(defn- find-smallest-neighbours
  "Given a key-path to a neigbouring region and a direction, return a list
  of the smallest regions neighbouring the region in the opposite direction."
  [tree neigbour-path dir]
  (when (nil? (get-in tree neigbour-path))
    (throw (Exception. (str "Path is nil: " (str neigbour-path)))))

  (loop [candidates [neigbour-path]
         neigbours []]
    (if (not-empty candidates)
      (let [first-candidate (first candidates)
            node (get-in tree first-candidate)
            is-leaf (tree-is-leaf node)
            new-neighbours (if (and is-leaf node) (conj neigbours first-candidate) neigbours)
            new-candidates (if (or is-leaf (nil? node))
                             candidates
                             (concat candidates
                                     (map #(conj first-candidate %) ((opposite-direction dir) dir->regions))))]

        (recur (rest new-candidates) new-neighbours))
      neigbours)))

(defn all-neighbouring-regions [tree region-path]
  (->>
   (for [dir [::up ::left ::right ::down]]
     (let [neighbouring-region-large (neigbouring-regions-greater-or-equal-size tree region-path dir)
           smallest-neighbours (find-smallest-neighbours tree neighbouring-region-large dir)]
       smallest-neighbours))
   (mapcat identity)))

(defn closest-point
  "
  Algorithm: given a point, find all neighbouring regions. This limits the search space."
  [tree pt]
  ;; step 1. Find which region point is conained in
  ;; step 2: find all points in all neiougbours + self
  (let [containing-region-path (point->region-path tree pt)
        neihbouring-regions (all-neighbouring-regions tree containing-region-path)
        paths-to-search (conj neihbouring-regions containing-region-path)
        candidate-points  (->> paths-to-search
                               (map (fn [p] (get-in tree p)))
                               (mapcat ::points))]
    (->> candidate-points
         (sort-by (fn [a] (p/distance-sq pt a)))
         first)))

(defn distance-to-interval [x [a b]]
  (cond
    (< x a) (- a x)
    (> x b) (- x b)
    :else 0))

(defn distance-to-region
  "Computes the taxi-cab metric distance from pt to region. That is, the maximum distance
  of pt in x-direction and y-direction."
  [region pt]
  (if (inside-region? region pt)
    0
    (let [[x y] pt
          [a b] (::top-left region)
          w (::width region)
          h (::height region)
          x-dist (distance-to-interval x [a (+ a w)])
          y-dist (distance-to-interval y [b (- b h)])]
      (max x-dist y-dist))))

(defn- create-region-queue [tree pt]
  (pm/priority-map-keyfn
   (fn [t] (distance-to-region (::region t) pt)) [] tree))

(defn- create-sorted-points [pt]
  (sorted-set-by (fn [p1 p2]
                   (let [dist-diff (- (p/distance-sq p1 pt)
                                      (p/distance-sq p2 pt))]
                     (if (= dist-diff 0)
                       (compare p1 p2)
                       (if (< dist-diff 0) -1 1))))))

(defn closest-point-2
  "Find closest point in tree given pt.

  Algorithm: recursively sort subregions by distance to point, in a depth-first way. Collect point until either a point
  with distance zero is found, or there is no more subregions."
  [tree pt]
  (loop [region-queue (create-region-queue tree pt)
         points-so-far (create-sorted-points pt)]
    (if (empty? region-queue)
      (first points-so-far)
      (let [[current-key current-node] (peek region-queue)
            new-points (into points-so-far (::points current-node))
            best-point (first new-points)
            distance-best-point (if-let [best-point best-point] (p/distance-sq pt best-point) nil)
            distance-best-region (distance-to-region (::region current-node) pt)]
        (if (or
             (empty? region-queue)
             (< (if (nil? distance-best-point) 0 distance-best-point) distance-best-region)
             )
          (first points-so-far)
          (let [popped-queue (pop region-queue)
                subtrees (tree-subtrees current-node)
                new-queue  (into popped-queue (map (fn [t] [(conj current-key (::corner t)) (::tree t)]) subtrees))
                
                ]
            (recur new-queue new-points)))))))

;; ide: bruke denne til å returnere funksjon slik at man traverse fra nearest til farest
(defn tree->seq [tree]
  (tree-seq map? (fn [n] (filter some? [(::ul n) (::ur n) (::ll n) (::lr n)])) tree))

(defn random-tree [n]
  (let [pts (r/random-pts n [0.0 1] [0.0 1])]
    (reduce (fn [acc curr] (insert acc curr)) empty-tree pts)))

;; IDE: mulighet for å oppdatere posisjon til punkt
;; https://geidav.wordpress.com/2014/11/18/advanced-octrees-3-non-static-octrees/
;; algoritme noe sånt: flytt punkt opp til nærmeste parent som inneholder ny posisjon
;; og sett inn nærmest mulig ny posisjon
;; burde være mulig å gjøre effektivt?
(comment

  (let [tree (random-tree 100000)] (time (count (query-range tree [[0 0.5] [0 1]]))))
  (let [pts (r/random-pts 10000 [0 1] [0 1])]
    (time (count (filter (fn [p] (inside-range? [[0 0.5] [0 1]] p)) pts))))

  (let [t (random-tree 1000)]
    (for [pt (r/random-pts 100 [0 .1] [0 0.1])]
      (let [pa (point->region-path t pt)]
        (when (nil? (get-in t pa)) (println pa))))

    "done")

  (stest/check `region-center)
;;; dbg ^{:break/when (> (count (:points curr-tree)) 9)}

)
