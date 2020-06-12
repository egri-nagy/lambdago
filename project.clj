(defproject lambdago "2020.04.30"
  :description "Software package for the Igo Math course https://egri-nagy.github.io/igomath/"
  :url "https://github.com/egri-nagy/lambdago"
  :license {:name "MIT License"
            :url "none"
            :year 2019
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [com.bhauman/rebel-readline "0.1.4"]
                 [instaparse "1.4.10"]
                 [rhizome "0.2.9"]
                 [org.clojure/math.numeric-tower "0.0.4"]
                 [net.mikera/core.matrix "0.62.0"]
                 [kigen "19.08.05"]
                 [metasoarous/oz "1.6.0-alpha6"]]
  :plugins [[lein-cloverage "1.1.2"]
            [lein-kibit "0.1.8"]
            [lein-ancient "0.6.15"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "0.3.11"]]
  :main ^:skip-aot lgo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
