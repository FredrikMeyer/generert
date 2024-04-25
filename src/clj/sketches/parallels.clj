(ns sketches.parallels
  (:require [quil.core :as q]
            [quil.middleware :as m]
            [tools.lines :as l]
            [tools.random :as r]
            [tools.drawing :as d]))

(def w 1500)
(def h 1500)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100)
  (q/no-fill)
  {})

(defn create-line []
  (r/random-pts 2 [0 w] [0 h]))

(defn create-lines [n]
  [[[50 50] [50 850]]
   [[50 850] [850 850]]
   [[850 850] [850 50]]
   [[850 50] [50 50]]
   [[50 50] [50 850]]])

(defn
  ^{:test (fn [] (assert (= 1 (mod-sign 1))))}
  mod-sign
  "Returns +/- 1, depending on if i is 0/1 mod 2."
  [i]
  (dec (* 2 (mod i 2))))

(defn point-on-circle [center r theta]
  (let [[cx cy] center
        x (+ cx (* r (Math/sin theta)))
        y (+ cy (* r (Math/cos theta)))]
    [x y]))

(defn lines-on-annulus [r1 r2 n]
  (let [lines (for [i (range 0 n)]
                (let [dir (mod-sign i)
                      theta (/ (* q/TWO-PI i) n)
                      l1x (point-on-circle [cx cy] (if (= dir 1) r1 r2) theta)
                      l1y (point-on-circle [cx cy] (if (= dir 1) r2 r1) theta)]
                  [l1x l1y]))
        f-line (first lines)]
    (conj (vec lines) f-line)))

(defn wiggle-line [l]
  (let [[[a b] [c d]] l
        s [a b c d]
        [x y z w] (map (fn [u] (+ u (r/random -50 50))) s)]
    [[x y] [z w]]))

(defn scale-to-center [line k]
  (let [[[a b] [c d]] line
        at (- a cx)
        bt (- b cy)
        ct (- c cx)
        dt (- d cy)
        as (* at k)
        bs (* bt k)
        cs (* ct k)
        ds (* dt k)]
    [[(+ as cx) (+ bs cy)] [(+ cs cx) (+ ds cy)]]))

(defn create-line2 [i]
  (let [scale-factor (Math/pow 2 (/ (* -1 (- i (mod i 4))) 2))]
    (case (mod i 4)
      0
      (scale-to-center [[50 50] [50 850]] scale-factor)
      1
      (scale-to-center [[50 850] [850 850]] scale-factor)
      2
      (scale-to-center [[850 850] [850 50]] scale-factor)
      3
      (scale-to-center [[850 50] [50 50]] scale-factor))))

(defn draw []
  (q/stroke-weight 1)
  (q/stroke 100 15)

  ;; IDE: nærmere og nærmere mot sentrum
  ;; innfør randomness, wiggle wiggle

  ;; lines
  ;; [[[50 100] [150 50]] [[200 300] [300 180]] [[300 300] [350 200]]]
  ;; (take 10 (repeatedly create-line))
  ;; (create-lines 10)
  ;; (map wiggle-line (create-lines 10))
  ;; (map wiggle-line (map create-line2 (range 0 10 1)))
  (comment
    (let [;; lines (map wiggle-line (map create-line2 (range 0 10 1)))
          lines (map identity (lines-on-annulus 50 400 10))
          lines (shuffle lines)
          line-pairs (partition 2 1 lines)]
      (doseq [[[l1 l2] [ll1 ll2]] line-pairs]
        (q/line l1 l2)
        (doseq [t (range 0 1.01 0.01)]
          (let [p1 (l/point-on-line l1 l2 t)
                p2 (l/point-on-line ll1 ll2 t)]
            (q/line p1 p2)))
        (q/line ll1 ll2))))

  (doseq [i (range (+ (- cx) 350) (- cx 150) 450)
          j (range (+ (- cy) 300) (- cy 150) 450)]
    (println "i" i "x" (/ (- i) 1) (+ i j))
    (q/with-translation [i j]
      (let [;; lines (map wiggle-line (map create-line2 (range 0 10 1)))
            lines (map identity (lines-on-annulus (q/map-range (+ i j) -1100 (+ w h) -100 150) 150 15))
            ;; lines (shuffle lines)
            lines (map wiggle-line lines)
            line-pairs (partition 2 1 lines)]
        (println "inner r" (q/map-range (+ i j) -100 300 -50 150))
        (doseq [[[l1 l2] [ll1 ll2]] line-pairs]
          ;; (q/with-stroke [45 100 90 50])
          (q/line l1 l2)
          (doseq [t (range 0 1.01 0.01)]
            (let [p1 (l/point-on-line l1 l2 t)
                  p2 (l/point-on-line ll1 ll2 t)]
              (if (> (rand) 0.0)
                (q/line p1 p2)
                (q/with-stroke [45 100 90 50]
                  (q/line p1 p2)))))

          (q/line ll1 ll2)))))

  (comment
    (q/line 100 100 700 100)
    (let [[[a b] [c d]] (scale-to-center [[100 100] [700 100]] 0.5)]
      (q/line a b c d))))

(defn update-state [state]
  state)

(defn draw-state [state]
  (q/background 0)
  (time (draw))
  (draw)
  ;; (println "Done")
  (q/no-loop)

  (q/frame-rate 0.5))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn sketch []
  (q/defsketch #_:clj-kondo/ignore texture
    :title "You spin my circle right round"
    :size [w h]
    :setup setup
    :update update-state
    :mouse-clicked (d/save-on-click-handler "parallels")
    :key-pressed d/redraw
    :draw draw-state
    :features [:keep-on-top :no-bind-output :pause-on-error]
    :middleware [m/fun-mode m/pause-on-error]))
