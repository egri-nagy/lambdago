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
  (let [command (first args)
        numargs (count args)]
    (println (str "LambdaGo v" (version/get-version "lambdago" "lambdago")) )
    (case command
      "gtp" (when (= numargs 2)
              (gtp-loop (second args)))
      "lizzie" (if (= numargs 2)
                 (do
                   (oz/start-server!)
                   (oz/view! (sgf-report (slurp (second args))) :mode :vega))
                 (println "Usage: lizzie sgf-file"))
      "katago-input" (if (= numargs 4)
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
      "script" (load-file (second args))
      nil (println "Available commands: gtp lizzie katago katago-input script"))
    (shutdown-agents)))
