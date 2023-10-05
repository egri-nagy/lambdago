(ns lgo.core
  (:gen-class)
  (:require [lgo.gtp :refer [gtp-loop]]
            [lgo.analysis.lizzie :refer [sgf-report]]
            [lgo.analysis.katago :refer [katago-output process-sgf]]
            [lgo.analysis.clay :refer [game-report]]
            [lgo.sgf :refer [simplified-sgf-string]]
            [lgo.util :refer [filename-without-extension]]
            [trptcolin.versioneer.core :as version]
            [clojure.java.io :as jio]))

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
                   ;(oz/start-server!)
                   ;(oz/view! (sgf-report (slurp (second args))) :mode :vega))
                 )
                 (println "Usage: lizzie sgf-file"))
      "lizzie-export" (if (= numargs 2)
                        (let [filename (second args)]

                          ;; (oz/export! (sgf-report (slurp filename))
                          ;;             (str
                          ;;              (filename-without-extension filename)
                          ;;              ".html")))
                        )
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
      "katago" (if (= numargs 2) 
                 (game-report (katago-output (second args)) (second args)) 
                 (println "Usage: katago analysis-output-file"))
      "cops" (if (>= numargs 2)
               (let [raws (into {} (map
                                    (fn [filename]
                                      [(filename-without-extension
                                        (.getName (clojure.java.io/file filename)))
                                       (katago-output filename)])
                                    (rest args)))]
                 (do
                  ;;  (oz/start-server!)
                  ;;  (oz/view! (cop-fingerprints raws)
                  ;;            :mode :vega)))
                 ))
               (println "Usage: cops analysis-output-file"))
        "cops-export" (if (>= numargs 2)
                 (let [raws (into {} (map
                                      (fn [filename]
                                        [(filename-without-extension
                                          (.getName (clojure.java.io/file filename)))
                                         (katago-output filename)])
                                      (rest args)))]
                   (do 
                    ;;  (oz/export! (cop-fingerprints raws)
                    ;;              "cop-fingerprints.html")))
                   ))
                 (println "Usage: cops analysis-output-file"))
      "katago-export"
      (do
        (if (= numargs 2)
          (let [filename (second args)]  
            ;; (oz/export! (game-report (katago-output filename)
            ;;                          filename)
            ;;             (str
            ;;              (filename-without-extension filename)
            ;;              ".html"))))
            
            (println "Usage: katago-export analysis-output-file"))))
     ;; for executing clojure code as a script
      "script" (load-file (second args))
      nil (println "Available commands: verson gtp simplify-sgf lizzie lizzie-export katago-input katago katago-export script")))
    (shutdown-agents))
