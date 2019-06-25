(defproject lambdago "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "none"
            :year 2019
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :main ^:skip-aot lambdago.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
