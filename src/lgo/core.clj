(ns lgo.core
  (:gen-class)
  (:require [lgo.board :refer :all] ;;for repl convenience
            [lgo.game :refer :all]
            [lgo.sgf :refer :all]
            [lgo.grid :refer :all]
            [lgo.gtp :refer :all]
            [lgo.lizzie :refer :all]
            [oz.core :as oz]))

;; to save compile time property into a runtime one
(defmacro get-version []
  (System/getProperty "lambdago.version"))

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (let [command (first args)]
    (case command
      "gtp" (do
              (println "LambdaGo"
                       (get-version))
                                        ;(load-file (first args))
              (gtp-loop))
      "lizzie" (do
                 (oz/start-server!)
                 (oz/view! (oz-effects (slurp (second args))) :mode :vega)))
    (shutdown-agents)))
