(ns lgo.core
  (:gen-class)
  (:require [lgo.gtp :refer [gtp-loop]]
            [lgo.analysis.lizzie :refer [sgf-report]]
            [lgo.analysis.katago :refer [katago-output process-sgf]]
            [lgo.analysis.oz :refer [game-report]]
            [lgo.sgf :refer [simplified-sgf-string]]
            [oz.core :as oz]
            [trptcolin.versioneer.core :as version]))

(defn -main
  "The first argument is a command."
  [& args]
  (let [command (first args)
        numargs (count args)]
    (case command
      "version"  (println (str "LambdaGo v"
                               (version/get-version "lambdago" "lambdago")))
      "gtp" (when (= numargs 2)
              (gtp-loop (second args)))
      "simplify-sgf" (if (= 2 numargs)
                       (println (simplified-sgf-string (slurp (second args))))
                       (println "Usage: simplify-sgf sgf-file"))
      "lizzie" (if (= numargs 2)
                 (do
                   (oz/start-server!)
                   (oz/view! (sgf-report (slurp (second args))) :mode :vega))
                 (println "Usage: lizzie sgf-file"))
      "katago-input" (if (>= numargs 3)
                       (let [sgf_file (second args)
                             max-visits (read-string (nth args 2))]
                         (if (= numargs 4)
                           (process-sgf sgf_file max-visits (read-string (nth args 3)))
                           (process-sgf sgf_file max-visits)))
                       (println "Usage: katago-input"
                                "sgf-file maxvisits [passed-maxvisits]"))
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
