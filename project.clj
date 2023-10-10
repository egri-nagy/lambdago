(defproject org.clojars.egri-nagy/lambdago "2023.10.05"
  :description "Software package for the Igo Math course https://egri-nagy.github.io/igomath/"
  :url "https://github.com/egri-nagy/lambdago"
  :license {:name "MIT License"
            :url "none"
            :year 2022
            :key "mit"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/tools.reader "1.3.6"] ;why? who needs this?
                 [instaparse "1.4.12"]
                 [org.clojure/math.numeric-tower "0.0.5"]
                 [org.clojure/data.json "2.4.0"]
                 [trptcolin/versioneer "0.2.0"]
                 [meander/epsilon "0.0.650"]
                 [org.scicloj/clay "2-alpha38"]]
  :plugins [[lein-cloverage "1.2.4"]
            [lein-kibit "0.1.8"]
            [lein-ancient "0.7.0"]
            [jonase/eastwood "1.4.0"]
            [fourtytoo/record-deps "0.3.3-SNAPSHOT"]
            [cider/cider-nrepl "0.39.0"]]
  :main lgo.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot [lgo.core]}})
