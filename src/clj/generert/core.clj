(ns generert.core)

(defn -main [& args]
  (let [cmd (first args)]
    (case cmd
      "pillars" (use 'pillars.core)
      "supershape" (use 'supershape.core)
      "texture" (use 'texture.core)
      "default"))
  (println "Done main file"))
