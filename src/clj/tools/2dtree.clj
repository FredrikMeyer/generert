(ns tools.2dtree
  (:require
   [tools.points :as p]
   [tools.random :as r]
   [clojure.pprint]))

(defprotocol I2DTree
  (value [this])
  (intersect-rect [this rect])
  (nearest [this pt]))

(defprotocol IDistance
  (distance-squared [this p]))

(defprotocol IIntersectable
  (contains-point? [this pt])
  (intersects-shape? [_ s]))

(defrecord Circle [center ^double radius])

(defrecord Rectangle [^double xmin ^double ymin ^double xmax ^double ymax]
  IIntersectable
  (contains-point? [_ [^double x ^double y]]
    (and (>= x xmin)
         (<= x xmax)
         (>= y ymin)
         (<= y ymax)))
  (intersects-shape? [_ s]
    (cond (instance? Rectangle s)
          (not (or (> ymin (:ymax s))
                   (< ymax (:ymin s))
                   (> xmin (:xmax s))
                   (< xmax (:xmin s))))
          :else (throw (Exception. "Unsupported intersection"))))
  IDistance
  (distance-squared [this [^double x ^double y :as p]]
    (if (contains-point? this p) 0
        (let [x-dist (cond (> x xmax)
                           (- xmax x)
                           (< x xmin)
                           (- xmin x)
                           :else 0)
              y-dist (cond (> y ymax)
                           (- ymax y)
                           (< y ymin)
                           (- ymin y)
                           :else 0)]
          (+ (* x-dist x-dist) (* y-dist y-dist))))))

(extend-type Circle
  IIntersectable
  (contains-point? [{:keys [center radius]} pt]
    (<= (p/distance-sq pt center) radius))
  (intersects-shape? [{:keys [center radius]} s]
    (cond (instance? Rectangle s)
          (<= (distance-squared s center) radius)
          (instance? Circle s)
          ;; hvordan fikse referanser slik??
          (<= (p/distance-sq center (:center s)) (+ radius (:radius s))))))

(defn left-of [^Rectangle rect [^double x :as pt]]
  (when-not (contains-point? rect pt)
    (throw (Exception. "pt must be inside rect")))
  (-> rect
      (assoc :xmax x)))

(defn right-of [^Rectangle rect [^double x :as pt]]
  (when-not (contains-point? rect pt)
    (throw (Exception. "pt must be inside rect")))
  (-> rect
      (assoc :xmin x)))

(defn top-of [^Rectangle rect [_ ^double y :as pt]]
  (when-not (contains-point? rect pt)
    (throw (Exception. "pt must be inside rect")))
  (-> rect
      (assoc :ymin y)))

(defn below-of [^Rectangle rect [_ ^double y :as pt]]
  (when-not (contains-point? rect pt)
    (throw (Exception. "pt must be inside rect")))
  (-> rect
      (assoc :ymax y)))

;; Copied from here: https://github.com/clojure/data.finger-tree/blob/master/src/main/clojure/clojure/data/finger_tree.clj
(defn- seq-equals [a b]
  (boolean
    (when (or (sequential? b) (instance? java.util.List b))
      (loop [a (seq a)
             b (seq b)]
        (when (= (nil? a) (nil? b))
          (or
            (nil? a)
            (when (= (first a) (first b))
              (recur (next a) (next b)))))))))

(defrecord TreeNode [value vertical rect])

(defn- tree-cons [root pt]
  (loop [path []
         vertical true]
    ;; If the current path exists
    (if-let [{:keys [value]} (get-in root path)]
      (let [comparator-fn (if vertical first second)
            current-compare-res (<= (comparator-fn pt) (comparator-fn value))]
        (cond
          ;; If pt already in tree, just return the tree
          (= value pt)
          root
          ;; If pt is lower than current value, recur with :lower appended to path
          current-compare-res
          (recur (conj path :lower) (not vertical))
          :else
          (recur (conj path :higher) (not vertical))))
      ;; We are in the case of a non-existing node
      ;; If path is empty, it means the tree was nil. Return a root node.
      (if (empty? path)
        (assoc
         (->TreeNode pt vertical (->Rectangle 0 0 1 1)) :size 1)
        ;; Otherwise, insert a new node at the current path
        (let [{prev-pt :value prev-rect :rect} (get-in root (pop path))
              curr-key (peek path)]
          (-> root
              (update :size inc)
              (assoc-in path
                        (->TreeNode pt vertical
                                    (if vertical
                                      (if (= curr-key :lower)
                                        (below-of prev-rect prev-pt)
                                        (top-of prev-rect prev-pt))
                                      (if (= curr-key :lower)
                                        (left-of prev-rect prev-pt)
                                        (right-of prev-rect prev-pt)))))))))))

(defn- worth-exploring?
  "Is the `rect` worth exploring when looking for pt?"
  [rect best-so-far pt]
  (< (distance-squared rect pt) (p/distance-sq pt best-so-far)))

(defn- tree-nearest
  "Find the nearest pt in the tree represented by root."
  [root pt]
  (if (nil? root) nil
      (loop [best-so-far (:value root)
             paths [[]]]
        (let [current-path (peek paths)
              {:keys [value lower higher vertical] :as current-node} (get-in root current-path)
              best-so-far* (min-key #(p/distance-sq pt %) value best-so-far)]
          (cond
            ;; The stack of paths to be explored is empty, return best-so-far
            (nil? current-path)
            best-so-far
            ;; If pt = value, then no need to do anything more
            (= pt value)
            value
            ;; Both children exist
            (and lower higher)
            (let [comparator-fn (if vertical first second)
                  current-compare-res (<= (comparator-fn pt) (comparator-fn value))
                  ;; Explore closest node first
                  child-nodes  (if current-compare-res '(:higher :lower) '(:lower :higher))
                  v (->> child-nodes
                         ;; Filter nodes worth exploring
                         (transduce (comp (filter #(worth-exploring? (:rect (% current-node)) best-so-far* pt))
                                          (map #(conj current-path %))) conj (pop paths)))]
              (recur best-so-far* v))
            (some? lower)
            (if (worth-exploring? (:rect lower) best-so-far* pt)
              (recur best-so-far* (conj (pop paths) (conj current-path :lower)))
              (recur best-so-far* (pop paths)))
            (some? higher)
            (if (worth-exploring? (:rect higher) best-so-far* pt)
              (recur best-so-far* (conj (pop paths) (conj current-path :higher)))
              (recur best-so-far* (pop paths)))
            :else
            (recur best-so-far* (pop paths)))))))

(defn- tree-insersect-rect [root other-rect]
  (if (nil? root) #{}
      (loop [points #{}
             paths [[]]]
        (if (empty? paths)
          points
          (let [current-path (peek paths)
                {:keys [value] :as current-node} (get-in root current-path)]
            (recur (if (contains-point? other-rect value) (conj points value) points)
                   (apply conj (pop paths)
                          (->> [:higher :lower]
                               (filter (fn [t] (when-let [child-node (t current-node)]
                                                 (intersects-shape? other-rect (:rect child-node)))))
                               (map #(conj current-path %))))))))))

(deftype TwoTree [^TreeNode root]
  I2DTree
  (value [_]
    (:value root))
  (intersect-rect [_ other-rect]
    (tree-insersect-rect root other-rect))

  (nearest [_ pt]
    (tree-nearest root pt))

  clojure.lang.ISeq
  (first [_]
    (letfn [(first* [{:keys [lower value]}]
              (if lower (recur lower) value))]
      (first* root)))
  (cons [_ pt]
    (TwoTree. (tree-cons root pt)))

  (next [this]
    (seq (.more this)))

  (more [_]
    (letfn [(more* [{:keys [lower higher]} path]
              (cond
                lower (recur lower (conj path :lower))
                (seq path) (TwoTree. (assoc-in root path higher))
                :else (TwoTree. higher)))]
      (more* root [])))

  clojure.lang.Sequential
  clojure.lang.Seqable
  (seq [this]
    (when (contains? root :value) this))

  clojure.lang.IPersistentCollection
  (equiv [this other]
    (seq-equals this other)
    )
  (empty [_]
    (TwoTree. nil))

  clojure.lang.Counted
  (count [_]
    (get root :size 0))

  clojure.lang.IPersistentSet
  (disjoin [_ _]
    (throw (Exception. "Not supported")))

  (contains [this pt]
    (boolean (get this pt)))

  (get [_ pt]
    (if (nil? root) nil
        (loop [path []]
          (if-let [{:keys [value ^boolean vertical]} (get-in root path)]
            (let [comparator-fn (if vertical second first)
                  current-compare-res (<= (comparator-fn pt) (comparator-fn value))]
              (cond (= value pt) value
                    current-compare-res
                    (recur (conj path :lower))
                    :else
                    (recur (conj path :higher))))
            nil))))

  Object
  (toString [_]
    (str "Tree" " " root)))

;; to avoid crash
;; See this issue https://github.com/thi-ng/color/pull/11
(prefer-method clojure.pprint/simple-dispatch clojure.lang.ISeq clojure.lang.IPersistentSet)

(defmethod print-method TwoTree [^TwoTree tree ^java.io.Writer w]
  (.write w (str "Tree" " " (.root tree))))

(defn two-tree [& xs]
  (reduce conj (TwoTree. nil) xs))

(comment
  ;; Preserves metadata
  (meta (first (let [t (two-tree) m {:a :b} p (with-meta [0.5 0.5] m)] (conj t p)))))

(comment
  (let [pts (r/random-pts 1000000 [0 1] [0 1])
        built-tree (into (two-tree) pts)
        nearest-tree (time (nearest built-tree [0.5 0.5]))
        nearest-sort (time (-> (sorted-set-by (fn [p q] (compare (p/distance-sq p [0.5 0.5]) (p/distance-sq q [0.5 0.5]))))
                               (into pts)
                               (first)))]
    (println "Nearest tree: " nearest-tree)
    (println "Nearest sort " nearest-sort)
    (println "-"))
;;
  )

(comment

  (let [pts (r/random-pts 10000 [0 1] [0 1])
        built-tree (into (two-tree) pts)
        intersected (intersect-rect built-tree (->Rectangle 0 0 0.5 0.5))]
    (println "Intersected tree: " (count intersected) "Tree " (str built-tree))

    (println "-"))
;;
  )

(comment
  (require '[clj-async-profiler.core :as prof])
  (let [pts (r/random-pts 1000000 [0 1] [0 1])]
    (prof/profile (into (two-tree) pts)))
  (prof/serve-ui 8080))

;; (fn [p q] (compare (p/distance-sq p [0.5 0.5]) (p/distance-sq q [0.5 0.5]))) (apply list (into (two-tree) (r/random-pts 1000 [0 1] [0 1])))
(comment

  (def a-tree (TwoTree. nil))

  (value a-tree)

  (def another-tree (conj a-tree [0 0] [1 2] [3 4]))
  another-tree)

(comment)

(comment
  (.getMethods clojure.lang.ISeq))

(comment
  (defn scaffold [iface]
    (doseq [[iface methods] (->> iface .getMethods
                                 (map #(vector (.getName (.getDeclaringClass %))
                                               (symbol (.getName %))
                                               (count (.getParameterTypes %))))
                                 (group-by first))]
      (println (str "  " iface))
      (doseq [[_ name argcount] methods]
        (println
         (str "    "
              (list name (into ['this] (take argcount (repeatedly gensym))))))))))

(comment

  (deftype AtomHash [val]
    Object
    (toString [_] (str "<AtomHash " @val ">"))
    clojure.lang.IPersistentMap
    clojure.lang.ILookup
    (valAt [_ key] (get @val key))
    (valAt [_ key notfound] (get @val key notfound))
    clojure.lang.IPersistentCollection
    (count [_] (.count @val))
    (empty [_] {})
    (cons [_ e] (.cons @val e))
    (equiv [this gs] (or (identical? this gs)
                         (when (identical? (class this) (class gs))
                           (= val (.val gs)))))
    clojure.lang.Associative
    (containsKey [_ k] (or (and (get @val k) true) false))
    (entryAt [_ k] (get @val k))
    clojure.lang.Seqable
    (seq [_] (seq @val))
    clojure.lang.IPersistentMap
    (assoc [_ k g] (assoc @val k g))
    (assocEx [this k g] (assoc this k g))
    (without [_ k] (.without @val k))
    clojure.lang.IDeref
    (deref [_] @val)))
;;REPLACE namespace with implementation namespace
(comment (defmethod print-dup AtomHash [o w]
           (.write w "#=(util/atom-hash ") (print-dup @o w) (.write w ")")))
