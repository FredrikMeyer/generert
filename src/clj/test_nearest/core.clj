(ns test_nearest.core
  (:require [quil.core :as q]
            [tools.random :as r]
            [tools.quadtree :as quad]
            [tools.drawing :as d]
            [tools.points :as p]
            [quil.middleware :as m]))

(def w 700)
(def h 700)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/no-fill)
  (q/frame-rate 0.5)
  ;; (q/rect-mode :center)

  {})

(def colors [[(* 100 (/ 41 365)) 16 96 100]
             [(* 100 (/ 177 365)) 32 73 100]
             [(* 100 (/ 177 365)) 26 36 100]
             [(* 100 (/ 361 365)) 57 91 100]])

(defn draw [s]
  ;; generate tree
  ;; draw all points
  ;; for each point, draw line to nearest neigbours

  ;; need to iterate through tree
  (binding [quad/*tree-capacity* 10]
    (let [t
          (let [pts (for [x (range 0 1 0.03)
                          y (range 0 1 0.03)]
                      [x y])]
            (-> quad/empty-tree
                (quad/insert-many pts)))]
      (let [points (mapcat :tools.quadtree/points (quad/tree->seq t))]
        (doseq [[x y] points]
          ;; [r1 r2] tilfeldig punkt i [[0 1] [0 1]]
          ;; [a b] skal være nærmeste punkt i treet
          (let [[r1 r2] (r/random-pt [0 1] [0 1])
                [a b] (quad/closest-point-2 t [r1 r2])]
            ;; Tegn linje fra tilfeldig punkt til [a b]
            (q/line (p/mult w [r1 (- 1 r2)]) (p/mult h [a (- 1 b)]))
            ;; Tegn en liten ellipse rundt [r1 r2]
            (q/with-fill [100 0 100]
              (q/with-stroke [100 100 100]
                (q/ellipse (* w r1) (- h (* h r2)) 3 3)))

            ;; Tegn [x y] (punkt i treet)
            (q/ellipse (* x w) (- h (* y h)) 3 3))))

      ;; Visualiser treet
      (let [nodes (quad/tree->seq t)]
        (doseq [node nodes]
          (let [r (:tools.quadtree/region node)]
            (let [[x y] (:tools.quadtree/top-left r)
                  ww (:tools.quadtree/width r)
                  hh (:tools.quadtree/width r)]
              (q/with-stroke [100 100 100 60]
                ;; (println r (* x w) (- h (* y h)) (* ww w) (* hh h))
                (q/stroke-weight (* 5 ww))
                (q/rect (* x w) (- h (* y h)) (* ww w) (* hh h))))
            #_(println r))))))

  (comment
    (let [t (quad/random-tree 1000)
          points (mapcat :tools.quadtree/points (quad/tree->seq t))]
      ;; (println (count points))
      ;; (println t)

      (doseq [pt (r/random-pts 100 [0 1] [0 1])]
        (let [n (quad/nearest-neighbours t pt 5)]

          (if n
            (doseq [nn n]
              (let [[x y] nn
                    [a b] pt]
                (q/rect (* w a) (* h b) 5 5)
                (q/ellipse (* w x) (* h y) 10 10))
              (q/line (p/mult w pt) (p/mult w nn)))

            (println "no neighbour"))))

      (comment
        (doseq [[x y] points]

          (let [[_ nei nei2] (quad/nearest-neighbours t [x y] 3)]
            ;; (println nei)
            (when (not (nil? nei))
              (q/line [(* x w) (* y h)] (p/mult w nei)))
            (when (not (nil? nei2))
              (q/line [(* x w) (* y h)] (p/mult w nei2)))))))))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time
   (draw state))
  ;; (time (draw))
  ;; (println "Done")
  (q/no-loop))

(q/defsketch #_:clj-kondo/ignore books
  :title "You spin my circle right round"
  :size [w h]
  :setup setup
  :update update-state
  :mouse-clicked (d/save-on-click-handler "books")
  :key-pressed d/redraw
  :draw draw-state
  :features [:keep-on-top :no-bind-output :pause-on-error]
  :middleware [m/fun-mode m/pause-on-error])
