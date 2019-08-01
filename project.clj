(defproject lambdago "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "MIT License"
            :url "none"
            :year 2019
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.bhauman/rebel-readline "0.1.4"]
                 [instaparse "1.4.10"]
                 [rhizome "0.2.9"]]
  :plugins [[lein-cloverage "1.1.1"]
            [lein-kibit "0.1.7"]
            [lein-ancient "0.6.15"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "0.3.6"]]
  :main ^:skip-aot lgo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
