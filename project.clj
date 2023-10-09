(defproject generert "0.1.1-SNAPSHOT"
  :description "Generert..."
  :url "https://fredrikmeyer.net/"
  :main generert.core
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :java-source-paths ["src/java"]
  :source-paths ["src/clj"]
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.clojure-goes-fast/clj-async-profiler "1.0.3"]
                 [org.clojure/core.match "1.0.0"]
                 [org.clojure/test.check "1.1.1"]
                 [quil "4.3-SNAPSHOT"]])
