(ns lgo.core
  (:gen-class)
  (:require [lgo.gtp :refer [gtp-loop]]
            [lgo.analysis.lizzie :refer [sgf-report]]
            [lgo.analysis.katago :refer [katago-output process-sgf]]
            [lgo.analysis.oz :refer [game-report]]
            [oz.core :as oz]
            [trptcolin.versioneer.core :as version]))

(defn -main
  "The first argument is a command."
  [& args]
  (let [command (first args)]
    (println (str "LambdaGo v" (version/get-version "lambdago" "lambdago")) )
    (case command
      "gtp" (gtp-loop)
      "lizzie" (do
                 (oz/start-server!)
                 (oz/view! (sgf-report (slurp (second args))) :mode :vega))
      "katago-input" (if (= (count args) 4)
                       (process-sgf (second args)
                                    (read-string (nth args 2))
                                    (read-string (nth args 3)))
                       (println "Usage: katago-input"
                                "sgf-file maxvisits passed-maxvisits"))
      ;; for analyzing katago output
      "katago" (do
                 (oz/start-server!)
                 (oz/view! (game-report (katago-output (second args))
                                        (second args))
                           :mode :vega))
      ;; for executing clojure code as a script
      "script" (load-file (second args)))
    (shutdown-agents)))
