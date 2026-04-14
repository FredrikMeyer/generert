(ns benchmark-rd
  (:require [tech.v3.tensor :as dtt]
            [tech.v3.datatype :as dtype]
            [criterium.core :as crit]))

(def h 100)
(def w 100)

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

(defn -main []
  (println "Creating test tensors..." h "x" w)

  (def test-grid
    (dtt/compute-tensor [h w]
                       (fn [y x] (float (/ (+ y x) (+ h w))))
                       :float32))

  (println "\n=== Benchmarking Convolution with clone (" h "x" w ") ===")
  (crit/quick-bench
    (dtt/clone (convolve-2d test-grid c)))

  (println "\n=== Benchmarking Single Pixel Access ===")
  (crit/quick-bench
    (let [result (convolve-2d test-grid c)]
      (result 50 50)))

  (println "\n=== Benchmarking Tensor Creation ===")
  (crit/quick-bench
    (let [result (dtt/compute-tensor [h w]
                                     (fn [y x] (* y x))
                                     :float32)]
      ;; Force evaluation
      (result 50 50))))

(-main)
