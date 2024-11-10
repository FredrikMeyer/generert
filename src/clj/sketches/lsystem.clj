(ns sketches.lsystem
  (:require [clojure.string :as str]))

(def L {:axiom "F-F-F-F"
        :reductions {"F" "FF+[+F-F-F]-[-F+F+F]"}})

(def L12 {:axiom "F+F+F-F"
          :reductions {"F" "FF+[+F-F-F]-[-F+F+F]"}})

(def L2 {:axiom "X"
         :reductions {"F" "FF"
                      "X" "F-[[X]+X]+F[+FX]-X"}})

(defn grow [lsystem n]
  (loop [i n sentence (:axiom lsystem)]
    (if (> i 0)
      (let [splitted (str/split sentence #"")
            reductions (:reductions lsystem)]
        (recur (dec i)
               (apply str (map (fn [s] (get reductions s s)) splitted))))
      sentence)))
