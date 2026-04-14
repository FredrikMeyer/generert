# Reaction-Diffusion (Gray-Scott Model) Implementation Notes

## Overview

The Gray-Scott model simulates two chemicals (A and B) that diffuse and react. The classic reaction is:
- A + 2B → 3B (B autocatalyzes using A)
- B → P (B decays to inert product P)

## Equations

```
dA/dt = D_a * ∇²A - AB² + f(1-A)
dB/dt = D_b * ∇²B + AB² - (k+f)B
```

Where:
- `D_a`, `D_b` = diffusion rates (typically D_a = 1.0, D_b = 0.5)
- `∇²` = Laplacian (implemented via convolution)
- `f` = feed rate (how fast A is replenished)
- `k` = kill rate (how fast B is removed)
- `AB²` = reaction term

## Implementation Structure

### State

```clojure
{:grid-a (dtt/new-tensor [h w] :datatype :float32)  ; Chemical A (0.0 to 1.0)
 :grid-b (dtt/new-tensor [h w] :datatype :float32)} ; Chemical B (0.0 to 1.0)
```

### Laplacian Kernel

Already defined in your code:
```clojure
(def laplacian (dtt/->tensor
                [[0.05 0.2 0.05]
                 [0.2  -1  0.2]
                 [0.05 0.2 0.05]]))
```

### Update Step

```clojure
(defn update-reaction-diffusion [state]
  (let [{:keys [grid-a grid-b]} state

        ;; Diffusion (Laplacian convolution)
        lap-a (convolve-2d grid-a laplacian)
        lap-b (convolve-2d grid-b laplacian)

        ;; Parameters
        D-a 1.0
        D-b 0.5
        f 0.055  ; feed rate (try 0.01 to 0.1)
        k 0.062  ; kill rate (try 0.045 to 0.07)
        dt 1.0   ; time step

        ;; Reaction term: A*B²
        reaction (dfn/* grid-a (dfn/sq grid-b))

        ;; Update A: A + (D_a*∇²A - AB² + f(1-A))*dt
        new-a (dfn/+ grid-a
                     (dfn/* dt
                            (dfn/- (dfn/+ (dfn/* D-a lap-a)
                                          (dfn/* f (dfn/- 1.0 grid-a)))
                                   reaction)))

        ;; Update B: B + (D_b*∇²B + AB² - (k+f)B)*dt
        new-b (dfn/+ grid-b
                     (dfn/* dt
                            (dfn/+ (dfn/* D-b lap-b)
                                   reaction
                                   (dfn/* (- (+ k f)) grid-b))))]

    {:grid-a new-a
     :grid-b new-b}))
```

### Initialization

Start with A=1 everywhere, B=0 except for small random seed:

```clojure
(defn setup []
  (q/color-mode :rgb 255)
  {:grid-a (dtt/compute-tensor [h w]
             (fn [y x] 1.0)
             :datatype :float32)
   :grid-b (dtt/compute-tensor [h w]
             (fn [y x]
               ;; Random seed in center region
               (if (and (< (abs (- y cy)) 20)
                        (< (abs (- x cx)) 20))
                 (if (< (rand) 0.5) 1.0 0.0)
                 0.0))
             :datatype :float32)})
```

### Visualization

Convert chemical B concentration to grayscale or color:

```clojure
(defn draw [state]
  (let [updated-state (update-reaction-diffusion state)
        grid-b (:grid-b updated-state)

        ;; Convert to RGB image (visualize B as brightness)
        img-tensor (dtt/compute-tensor [h w 3]
                     (fn [y x c]
                       (let [b-val (grid-b y x)
                             intensity (* 255 (max 0 (min 1 b-val)))]
                         intensity))
                     :datatype :uint8)]

    (q/set-image 0 0 (processing.core.PImage.
                      (bufimg/tensor->image img-tensor)))

    ;; Return updated state for next frame
    updated-state))
```

## Interesting Parameter Combinations

Different (f, k) values create different patterns:

| Pattern | f | k | Description |
|---------|---|---|-------------|
| Spots | 0.035 | 0.065 | Stable spots |
| Stripes | 0.035 | 0.060 | Parallel stripes |
| Waves | 0.014 | 0.054 | Moving waves |
| Chaos | 0.026 | 0.051 | Chaotic patterns |
| Coral | 0.062 | 0.061 | Coral-like growth |
| Mitosis | 0.029 | 0.057 | Self-replicating spots |

## Performance Tips

1. Use `:float32` datatype instead of `:float64` for better performance
2. Consider updating state every N frames if real-time is slow
3. Can reduce resolution (e.g., 256x256) for faster iteration
4. Pre-compute constants outside the update function

## References

- [Karl Sims - Reaction-Diffusion Tutorial](http://www.karlsims.com/rd.html)
- [Processing Implementation](https://github.com/Softwave/GrayScott)
- [Interactive Parameter Explorer](https://mrob.com/pub/comp/xmorphia/)
