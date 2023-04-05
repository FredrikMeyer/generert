(ns books.core
  (:require [quil.core :as q]
            [tools.random :as r]
            [tools.drawing :as d]
            [quil.middleware :as m]))

(def w 1500)
(def h 900)

(def cx (/ w 2))
(def cy (/ h 2))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 100)
  (q/no-fill)
  (q/frame-rate 0.5)
  {})

(def colors [[(* 100 (/ 41 365)) 16 96 100]
             [(* 100 (/ 177 365)) 32 73 100]
             [(* 100 (/ 177 365)) 26 36 100]
             [(* 100 (/ 361 365)) 57 91 100]])

(defn get-book []
  {:width (r/random 10 30)
   :height (r/random 15 100)})

(defn draw-book [x y book]
  (let [b-width (:width book)
        b-height (:height book)
        g (q/create-graphics (inc b-width) (inc b-height))]
    (q/with-graphics g
      (q/color-mode :hsb 100 100 100 100)
      (q/stroke 100 100)
      (q/no-fill)
      ;; (apply q/fill (rand-nth colors))

      (q/with-fill (rand-nth colors)
        (q/rect 0 0 b-width b-height))
      (when (< (rand) 0.5)
        (doseq [y (range -20 100 10)]
          (q/line 0 y b-width (+ y 30)))))
    (q/image g x y)))

(defn bookshelf [{:keys [b-height b-width n-shelves max-books] :or {n-shelves 5 max-books 15}}]
  (let [frame-width 10
        feet-height 40
        inner-width (- b-width (* 2 frame-width))
        inner-height (- b-height (* frame-width))
        shelf-frame-height (/ frame-width 2)
        shelf-height (+ (/ inner-height n-shelves) (- shelf-frame-height))]
    (q/begin-shape)
    (doseq [p [[100 100] [(+ 100 b-width) 100]
               [(+ 100 b-width) (+ 100 b-height feet-height)]
               [(+ 100 b-width (- frame-width)) (+ b-height 100 feet-height)]
               [(+ 100 b-width (- frame-width)) (+ 100 b-height)]
               [(+ 100 frame-width) (+ b-height 100)]
               [(+ 100 frame-width) (+ b-height 100 feet-height)]
               [100 (+ b-height 100 feet-height)]]]
      (apply q/vertex p))

    (q/begin-contour)
    (doseq [p [[(+ 100 frame-width) (+ 100 frame-width)]
               [(+ 100 frame-width inner-width) (+ 100 frame-width)]
               [(+ 100 frame-width inner-width) (+ 100 frame-width inner-height)]
               [(+ 100 frame-width) (+ 100 frame-width inner-height)]]]
      (apply q/vertex p))
    (q/end-contour)

    (q/end-shape :close)

    (doseq [i (range 1 (inc n-shelves))]
      (let [y (+ 100 frame-width (* i (/ inner-height n-shelves)))
            right-to-left (if (< (rand) 0.5) true false)
            n-books (+ 4 (rand-int max-books))]

        ;; Draw shelf bottom
        (comment
          (q/with-fill [100 100]
            (q/text (str y) 20 y)))
        (q/rect (+ 100 frame-width) y inner-width shelf-frame-height)

                ;; Draw books
        (comment
          (q/with-fill [100 100]
            (q/text (str "n-b " n-books) 20 (+ y 20))))

        (loop [x-so-far 0
               n-books n-books]

          (let [book (get-book)
                space-between (if (< (rand) 0.3) (r/random 0 20) 1)
                b-width (:width book)
                b-height (:height book)
                x-pos (+ 100 frame-width x-so-far)
                x-pos-adjusted (if right-to-left
                                 (+ 100 frame-width inner-width (- b-width) (- x-so-far))
                                 x-pos)]
            (when (< (+ b-width x-so-far) inner-width)
              (if (< (+ b-height 5) shelf-height)

                (when (> n-books 0)
                  (draw-book x-pos-adjusted
                             (- y (:height book))
                             book)
                  (recur (+ x-so-far b-width space-between) (dec n-books)))
                (recur x-so-far n-books)))))))))

(defn draw []

  (q/with-translation [0 -50]
    (bookshelf {:b-height 500 :b-width 300 :n-shelves 6}))

  (q/with-translation [400 -50]
    (bookshelf {:b-height 300 :b-width 200 :n-shelves 3}))

  (q/with-translation [400 350]
    (bookshelf {:b-height 400 :b-width 150 :n-shelves 4}))

  (q/with-translation [700 -50]
    (bookshelf {:b-height 600 :b-width 400 :n-shelves 6}))

  (q/with-translation [700 620]
    (bookshelf {:b-height 100 :b-width 600 :n-shelves 1 :max-books 50}))

  (q/with-translation [0 550]
    (bookshelf {:b-height 100 :b-width 300 :n-shelves 1 :max-books 10}))

  (q/with-translation [1150 -50]
    (bookshelf {:b-height 100 :b-width 150 :n-shelves 1 :max-books 10}))

  (q/with-translation [1150 150]
    (bookshelf {:b-height 400 :b-width 150 :n-shelves 2 :max-books 10}))
  )

(defn update-state [state]
  state)

(defn draw-state [_]
  (q/background 0)
  (draw)
  ;; (time (draw))
  ;; (println "Done")
  ;; (q/no-loop)
  )


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
