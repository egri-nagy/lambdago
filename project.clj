;;OBSOLETE!!!
(defproject org.clojars.egri-nagy/lambdago "2023.10.05"
  :description "Software package for the Igo Math course https://egri-nagy.github.io/igomath/"
  :url "https://github.com/egri-nagy/lambdago"
  :license {:name "MIT License"
            :url "none"
            :year 2022
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.12.0"]
                 [org.clojure/tools.reader "1.5.0"] ;why? who needs this?
                 [io.github.clojure/tools.build "0.10.5"]
                 [instaparse "1.5.0"]
                 [org.clojure/math.numeric-tower "0.1.0"]
                 [org.clojure/data.json "2.5.0"]
                 [meander/epsilon "0.0.650"]
                 [org.scicloj/clay "2-beta23"]]
  :plugins [[lein-cloverage "1.2.4"]
            [lein-kibit "0.1.11"]
            [lein-ancient "0.7.0"]
            [jonase/eastwood "1.4.3"]
            [fourtytoo/record-deps "0.3.3-SNAPSHOT"]
            [cider/cider-nrepl "0.50.2"]]
  :main lgo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot [lgo.core]}})
