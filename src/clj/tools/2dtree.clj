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

(defprotocol IIntersectAble
  (contains-point? [this other])
  (intersects-shape [_ s]))

(defrecord Rectangle [^double xmin ^double ymin ^double xmax ^double ymax]
  IIntersectAble
  (contains-point? [_ [^double x ^double y]]
    (and (>= x xmin)
         (<= x xmax)
         (>= y ymin)
         (<= y ymax)))
  (intersects-shape [_ s]
    (cond (instance? Rectangle s)
                 (not (or (> xmin (:xmin s))
                          (> ymin (:ymin s))
                          (< xmax (:xmax s))
                          (< ymax (:ymax s))))
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

(defrecord TreeNode [value vertical rect])

(defn- tree-cons [root pt]
  (loop [path []
         vertical true]
    (if-let [{:keys [value] :as curr-node} (get-in root path)]
      (let [comparator-fn (if vertical first second)
            current-compare-res (<= (comparator-fn pt) (comparator-fn value))]
        (cond (= value pt) root
              current-compare-res
              (recur (conj path :lower) (not vertical))
              :else
              (recur (conj path :higher) (not vertical))))
      (if (empty? path)
        (assoc
         (->TreeNode pt vertical (->Rectangle 0 0 1 1)) :size 1)
        (let [{prev-pt :value prev-rect :rect :as prev-node} (get-in root (pop path))
              curr-key (peek path)]
          (-> root
              (update :size inc)
              (assoc-in path (->TreeNode pt vertical (if vertical
                                                       (if (= curr-key :lower)
                                                         (below-of prev-rect prev-pt)
                                                         (top-of prev-rect prev-pt))
                                                       (if (= curr-key :lower)
                                                         (left-of prev-rect prev-pt)
                                                         (right-of prev-rect prev-pt)))))))))))

(deftype TwoTree [root]
  I2DTree
  (value [_]
    (:value root))
  (intersect-rect [this other-rect]
    (if (nil? root) #{}
             (loop [points #{}
                    paths [[]]]
               (if (empty? paths)
                 points
                 (let [current-path (peek paths)
                       {:keys [value lower higher vertical rect] :as current-node} (get-in root current-path)]
                   (recur (if (contains-point? other-rect value) (conj points value) points)
                          (cond (and lower higher)
                                (conj (pop paths) (conj current-path :lower) (conj current-path :higher))
                                (some? lower)
                                (conj (pop paths) (conj current-path :lower))
                                (some? higher)
                                (conj (pop paths) (conj current-path :higher))
                                :else (pop paths))))))))

  (nearest [this pt]
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
                    current-compare-res (<= (comparator-fn pt) (comparator-fn value))]
                (if current-compare-res
                       ;; Explore closest children first
                  (recur best-so-far* (conj (pop paths) (conj current-path :higher) (conj current-path :lower)))
                  (recur best-so-far* (conj (pop paths) (conj current-path :lower) (conj current-path :higher)))))
              (some? lower)
              (if (< (distance-squared (:rect lower) pt) (p/distance-sq pt best-so-far*))
                (recur best-so-far* (conj (pop paths) (conj current-path :lower)))
                (recur best-so-far* (pop paths)))
              (some? higher)
              (if (< (distance-squared (:rect higher) pt) (p/distance-sq pt best-so-far*))
                (recur best-so-far* (conj (pop paths) (conj current-path :higher)))
                (recur best-so-far* (pop paths)))
              :else
              (recur best-so-far* (pop paths)))))))

  clojure.lang.ISeq
  (first [this]
    (letfn [(first* [{:keys [lower value]}]
              (if lower (recur lower) value))]
      (first* root)))
  (cons [this pt]
    (TwoTree. (tree-cons root pt)))

  (next [this]
    (seq (.more this)))
  (more [this]
    (letfn [(more* [{:keys [lower higher] :as node} path]
              (cond
                lower (recur lower (conj path :lower))
                (seq path) (TwoTree. (assoc-in root path higher))
                :else (TwoTree. higher)))]
      (more* root [])))

  clojure.lang.Seqable
  (seq [this]
    (when (contains? root :value) this))

  clojure.lang.IPersistentCollection
  (equiv [_ other]
    (= root (.root other)))
  (empty [_]
    (TwoTree. nil))

  clojure.lang.Counted
  (count [_]
    (if (nil? root) 0 (:size root)))

  clojure.lang.IPersistentSet
  (disjoin [this other]
    (throw (Exception. "Not supported")))

  (contains [this pt]
    #_(when (instance? clojure.lang.Keyword pt)
        (throw (Exception. "ops")))
    (if (nil? root) false
        (loop [path []]
          (if-let [{:keys [value ^boolean vertical] :as curr-node} (get-in root path)]
            (let [comparator-fn (if vertical second first)
                  current-compare-res (<= (comparator-fn pt) (comparator-fn value))]
              (cond (= value pt) true
                    current-compare-res
                    (recur (conj path :lower))
                    :else
                    (recur (conj path :higher))))
            false))))

  (get [this pt]
    ;; TODO search for pt
    pt)

  Object
  (toString [_]
    (str "Tree" " " root)))

clojure.lang.IMeta
with-meta
;; to avoid crash
;; See this issue https://github.com/thi-ng/color/pull/11
(prefer-method clojure.pprint/simple-dispatch clojure.lang.ISeq clojure.lang.IPersistentSet)

(defmethod print-method TwoTree [tree ^java.io.Writer w]
  (.write w (str "Tree" " " (.root tree))))

(defn two-tree []
  (TwoTree. nil))

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
    (toString [this] (str "<AtomHash " @val ">"))
    clojure.lang.IPersistentMap
    clojure.lang.ILookup
    (valAt [this key] (get @val key))
    (valAt [this key notfound] (get @val key notfound))
    clojure.lang.IPersistentCollection
    (count [this] (.count @val))
    (empty [this] {})
    (cons [this e] (.cons @val e))
    (equiv [this gs] (or (identical? this gs)
                         (when (identical? (class this) (class gs))
                           (= val (.val gs)))))
    clojure.lang.Associative
    (containsKey [this k] (or (and (get @val k) true) false))
    (entryAt [this k] (get @val k))
    clojure.lang.Seqable
    (seq [this] (seq @val))
    clojure.lang.IPersistentMap
    (assoc [this k g] (assoc @val k g))
    (assocEx [this k g] (assoc this k g))
    (without [this k] (.without @val k))
    clojure.lang.IDeref
    (deref [this] @val)))
;;REPLACE namespace with implementation namespace
(comment (defmethod print-dup AtomHash [o w]
           (.write w "#=(util/atom-hash ") (print-dup @o w) (.write w ")")))
