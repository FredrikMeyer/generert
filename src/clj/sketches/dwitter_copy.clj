(ns sketches.dwitter-copy
  (:require
   [quil.core :as q]
   [template.dynamic :as dyn]))

(def w 800)
(def h 800)

(def cx (/ w 2))
(def cy (/ h 2))

;; https://www.dwitter.net/d/33924

(reset! dyn/loop? true)
(reset! dyn/update-state-fn (fn [state]
                              (-> state
                                  (update :t inc))))

(defn setup []
  (q/color-mode :hsb 100 100 100 100)
  (q/stroke 100 50)
  (q/no-fill)
  {:t 0})

#_(defn j [i]
    (bit-shift-right i (int (/ i 2000))))

(defn j [i]
  (bit-and i (bit-shift-right i 8)))

(defn angles [[prev-x prev-y i] t]
  (let [i' (dec i)
        j' (j i')
        a (+ prev-y j')
        b (+ prev-x (* j' (+ 9 (/ t 5999.0))))]
    [(+ (Math/cos a) (Math/cos b))
     (+ (Math/sin a) (Math/sin b))
     i']))

(defn draw [{t :t}]
  (let [f (fn [z] (angles z t))]
    (doseq [[x y _] (take 60000 (iterate f [300000 30000 30000]))]
      (let [screen-x (+ cx (* 180 x))
            screen-y (+ cy (* 180 y))]
        ;; (q/stroke (q/map-range screen-x 0 w 0 100) 70 60)
        (q/point screen-x screen-y)))))

"const canvas = document.querySelector('canvas');
const x = canvas.getContext('2d');
let X = 3e4, Y = 3e4, i = 3e4;

while (i--) {
    x.clearRect(0, 0, canvas.width, canvas.height);
    x.fillRect(
        960 + 240 * X,
        540 + 240 * Y,
        1,
        0.3
    );

    const j = i >> (i / 2000);
    const a = Y + j;
    const b = X + j * (t/999 + 9);

    X = Math.cos(a) + Math.cos(b);
    Y = Math.sin(a) + Math.sin(b);
}

"
