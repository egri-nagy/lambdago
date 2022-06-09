(defproject lambdago "2022.06.09"
  :description "Software package for the Igo Math course https://egri-nagy.github.io/igomath/"
  :url "https://github.com/egri-nagy/lambdago"
  :license {:name "MIT License"
            :url "none"
            :year 2019
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [com.bhauman/rebel-readline "0.1.4"]
                 [instaparse "1.4.12"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [metasoarous/oz "2.0.0-alpha5"]
                 [org.clojure/data.json "2.4.0"]
                 [trptcolin/versioneer "0.2.0"]]
  :plugins [[lein-cloverage "1.2.4"]
            [lein-kibit "0.1.8"]
            [lein-ancient "0.7.0"]
            [lein-bikeshed "0.5.2"]
            [jonase/eastwood "1.2.3"]]
  :main lgo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :aliases {"rebl" ["trampoline" "run" "-m" "rebel-readline.main"]})
