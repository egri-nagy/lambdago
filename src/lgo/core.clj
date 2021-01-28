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
  "The first argument is a command."
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
      "katago-input" (do
                       (process-sgf (second args) (read-string (nth args 2))))
      ;; for analyzing katago output
      "katago" (do
                 (oz/start-server!)
                 (oz/view! (game-report (katago-output (second args))
                                        (second args))
                           :mode :vega))
      ;; for executing clojure code as a script
      "script" (load-file (second args)))
    (shutdown-agents)))
