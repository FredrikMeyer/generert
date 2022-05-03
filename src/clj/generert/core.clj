(ns generert.core)

(defn -main [& args]
  (let [cmd (first args)]
    (case cmd
      "pillars" (use 'pillars.core)
      "default"))
  (println "Done main file"))
