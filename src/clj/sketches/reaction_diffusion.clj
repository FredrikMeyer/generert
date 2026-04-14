(ns sketches.reaction_diffusion
  (:require [tech.v3.tensor :as dtt]
            [tech.v3.datatype :as dtype]
            [tech.v3.libs.buffered-image :as bufimg]
            [criterium.core :as crit])
  (:import [java.awt.image BufferedImage]
           [javax.imageio ImageIO]
           [java.io File]))

(def w 500)
(def h 500)

(def cx (/ w 2))
(def cy (/ h 2))

(def c (dtt/->tensor
        [[0.05 0.2 0.05]
         [0.2 -1 0.2]
         [0.05 0.2 0.05]]))

(defn convolve-2d [prev kernel]
  (let [[h w] (dtype/shape prev)
        [kh kw] (dtype/shape kernel)
        pad-y (quot kh 2)
        pad-x (quot kw 2)]
    (dtt/compute-tensor [h w]
                        (fn [y x]
                          (loop [sum 0.0
                                 ky 0
                                 kx 0]
                            (if (>= ky kh)
                              sum
                              (let [img-y (+ y ky (- pad-y))
                                    img-x (+ x kx (- pad-x))
                                    ;; Clamp to valid range
                                    img-y (max 0 (min img-y (dec h)))
                                    img-x (max 0 (min img-x (dec w)))
                                    pixel-val (prev img-y img-x)
                                    new-sum (+ sum (* (kernel ky kx) pixel-val))
                                    [next-ky next-kx] (if (>= (inc kx) kw)
                                                        [(inc ky) 0]
                                                        [ky (inc kx)])]
                                (recur new-sum next-ky next-kx)))))
                        :float32)))

(defn update-reaction-diffusion [state]
  (let [{:keys [grid-a grid-b]} state

        ;; Diffusion (Laplacian convolution) - eagerly realize to avoid lazy evaluation
        lap-a (dtt/clone (convolve-2d grid-a c))
        lap-b (dtt/clone (convolve-2d grid-b c))

        ;; Parameters
        D-a 1.0
        D-b 0.5
        f 0.055  ; feed rate (try 0.01 to 0.1)
        k 0.062  ; kill rate (try 0.045 to 0.07)
        dt 0.5   ; time step (reduced from 1.0 for stability)

        ;; Compute updates more efficiently to reduce intermediate tensors
        ;; For A: dA = (D_a*∇²A - AB² + f(1-A))*dt
        ;; For B: dB = (D_b*∇²B + AB² - (k+f)B)*dt

        new-a (dtt/clone
               (dtt/compute-tensor [h w]
                                   (fn [y x]
                                     (let [a (grid-a y x)
                                           b (grid-b y x)
                                           la (lap-a y x)
                                           reaction (* a b b)
                                           da (+ (* D-a la)
                                                 (- reaction)
                                                 (* f (- 1.0 a)))
                                           new-val (+ a (* dt da))]
                                       ;; Clamp to [0, 1] for stability
                                       (max 0.0 (min 1.0 new-val))))
                                   :float32))

        new-b (dtt/clone
               (dtt/compute-tensor [h w]
                                   (fn [y x]
                                     (let [a (grid-a y x)
                                           b (grid-b y x)
                                           lb (lap-b y x)
                                           reaction (* a b b)
                                           db (+ (* D-b lb)
                                                 reaction
                                                 (* (- (+ k f)) b))
                                           new-val (+ b (* dt db))]
                                       ;; Clamp to [0, 1] for stability
                                       (max 0.0 (min 1.0 new-val))))
                                   :float32))]

    {:grid-a new-a
     :grid-b new-b}))

(defn setup
  "Initialize reaction-diffusion system.
   Options:
   - :mode - :center-seed (default), :random-spots, or :uniform-noise
   - :noise-level - for :uniform-noise mode (default 0.5)"
  [& {:keys [mode noise-level]
      :or {mode :center-seed
           noise-level 0.5}}]
  {:grid-a (dtt/compute-tensor [h w]
                               (fn [y x] 1.0)
                               :float32)
   :grid-b (dtt/compute-tensor [h w]
                               (fn [y x]
                                 (case mode
                                   :center-seed
                                   ;; Concentrated seed in center region
                                   (if (and (< (Math/abs (- y cy)) 50)
                                           (< (Math/abs (- x cx)) 50)
                                           (< (rand) 0.3))
                                     1.0
                                     0.0)

                                   :random-spots
                                   ;; Random spots across entire domain
                                   (if (< (rand) 0.01)  ; 1% of pixels
                                     1.0
                                     0.0)

                                   :uniform-noise
                                   ;; Uniform noise across domain
                                   (if (< (rand) noise-level)
                                     (* 0.5 (+ 0.5 (rand)))  ; 0.5-1.0
                                     0.0)))
                               :float32)})

(defn state->image
  "Convert reaction-diffusion state to BufferedImage.
   Visualizes B chemical as color intensity."
  [state]
  (let [grid-a (:grid-a state)
        grid-b (:grid-b state)
        ;; Visualize: B in white/yellow, A as background
        ;; This makes patterns more visible
        img-tensor (dtt/compute-tensor [h w 3]
                                       (fn [y x c]
                                         (let [a-val (grid-a y x)
                                               b-val (grid-b y x)
                                               ;; Color mapping: high B = bright yellow/white
                                               ;; low B (high A) = dark blue/black
                                               intensity (cond
                                                          (= c 0) (* 255 b-val)           ; Red channel
                                                          (= c 1) (* 255 b-val)           ; Green channel
                                                          (= c 2) (* 255 (- 1.0 b-val)))  ; Blue channel (inverted)
                                               clamped (max 0 (min 255 intensity))]
                                           clamped))
                                       :uint8)]
    (bufimg/tensor->image img-tensor)))

(defn state->image-grayscale
  "Convert reaction-diffusion state to grayscale BufferedImage (B chemical only)."
  [state]
  (let [grid-b (:grid-b state)
        img-tensor (dtt/compute-tensor [h w 3]
                                       (fn [y x c]
                                         (let [b-val (grid-b y x)
                                               intensity (* 255 (max 0 (min 1 b-val)))]
                                           intensity))
                                       :uint8)]
    (bufimg/tensor->image img-tensor)))

(defn print-state-stats
  "Print statistics about the current state"
  [state]
  (let [grid-a (:grid-a state)
        grid-b (:grid-b state)
        a-vals (for [y (range h) x (range w)] (grid-a y x))
        b-vals (for [y (range h) x (range w)] (grid-b y x))
        a-min (apply min a-vals)
        a-max (apply max a-vals)
        a-mean (/ (apply + a-vals) (* h w))
        b-min (apply min b-vals)
        b-max (apply max b-vals)
        b-mean (/ (apply + b-vals) (* h w))]
    (println "Grid A - min:" a-min "max:" a-max "mean:" a-mean)
    (println "Grid B - min:" b-min "max:" b-max "mean:" b-mean)))

(defn save-image!
  "Save BufferedImage to PNG file"
  [^BufferedImage img filepath]
  (let [file (File. filepath)
        parent (.getParentFile file)]
    (when parent
      (.mkdirs parent))
    (ImageIO/write img "png" file)
    (println "Saved:" filepath)))

(defn run-simulation
  "Run reaction-diffusion simulation for n steps, saving frames periodically"
  [n-steps & {:keys [save-every output-dir]
              :or {save-every 100
                   output-dir "images/reaction_diffusion"}}]
  (loop [state (setup)
         step 0]
    (when (< step n-steps)
      (when (zero? (mod step save-every))
        (println (str "Step " step "/" n-steps))
        (let [img (state->image state)
              filepath (str output-dir "/frame_" (format "%05d" step) ".png")]
          (save-image! img filepath)))
      (recur (update-reaction-diffusion state) (inc step))))
  (println "Simulation complete!"))

(comment
  ;; === USAGE ===

  ;; Run simulation and save frames
  (require 'sketches.reaction-diffusion :reload)
  (in-ns 'sketches.reaction-diffusion)

  ;; Run 1000 steps, save every 100 frames
  (run-simulation 1000 :save-every 100)

  ;; Run longer simulation, save less frequently
  (run-simulation 5000 :save-every 500)

  ;; Custom output directory
  (run-simulation 1000 :save-every 100 :output-dir "images/rd_test")

  ;; === MANUAL TESTING ===

  ;; Test 1: Center seed (localized pattern - ring formation)
  (def test-center (setup :mode :center-seed))
  (def s1000 (nth (iterate update-reaction-diffusion test-center) 1000))
  (save-image! (state->image s1000) "test_center_1000.png")

  ;; Test 2: Random spots (BEST for full domain patterns)
  (def test-spots (setup :mode :random-spots))
  (print-state-stats test-spots)
  (def s2000 (nth (iterate update-reaction-diffusion test-spots) 2000))
  (print-state-stats s2000)
  (save-image! (state->image s2000) "test_spots_2000.png")

  ;; Test 3: Uniform noise
  (def test-noise (setup :mode :uniform-noise :noise-level 0.5))
  (def s3000 (nth (iterate update-reaction-diffusion test-noise) 3000))
  (save-image! (state->image s3000) "test_noise_3000.png")

  ;; Run long simulation to see full pattern development
  (time
   (let [initial (setup :mode :random-spots)
         final (nth (iterate update-reaction-diffusion initial) 5000)]
     (print-state-stats final)
     (save-image! (state->image final) "test_spots_5000.png")))

  ;; === BENCHMARKS ===

  ;; Create test data
  (def test-state (setup))
  (def test-grid (:grid-a test-state))

  ;; Benchmark convolution
  (println "\n=== Convolve 2D ===")
  (crit/quick-bench (convolve-2d test-grid c))

  ;; Benchmark full update
  (println "\n=== Full Update ===")
  (crit/quick-bench (update-reaction-diffusion test-state))

  ;; Benchmark tensor creation
  (println "\n=== Tensor Creation ===")
  (crit/quick-bench
   (dtt/compute-tensor [h w]
                       (fn [y x] (* y x))
                       :float32))

  ;; Benchmark tensor access
  (println "\n=== Tensor Access (10000 reads) ===")
  (crit/quick-bench
   (dotimes [_ 10000]
     (test-grid (rand-int h) (rand-int w))))

  )
