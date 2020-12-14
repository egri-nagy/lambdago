(ns lgo.core
  (:gen-class)
  (:require [lgo.board :refer :all] ;;for repl convenience
            [lgo.game :refer :all]
            [lgo.sgf :refer :all]
            [lgo.grid :refer :all]
            [lgo.gtp :refer :all]
            [lgo.stats :refer :all]
            [lgo.analysis.lizzie :refer :all]
            [lgo.analysis.katago :refer :all]
            [lgo.analysis.oz :refer :all]
            [oz.core :as oz]
            [trptcolin.versioneer.core :as version]))

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (let [command (first args)]
    (case command
      "gtp" (do
              (println (str "LambdaGo v"
                        (version/get-version "lambdago" "lambdago")))
                                        ;(load-file (first args))
              (gtp-loop))
      "lizzie" (do
                 (oz/start-server!)
                 (oz/view! (sgf-report (slurp (second args))) :mode :vega))
      "katago" (do
                 (oz/start-server!)
                 (oz/view! (game-report (katago-output (second args))
                                        (second args))
                           :mode :vega))
      "script" (load-file (second args)))
    (shutdown-agents)))
