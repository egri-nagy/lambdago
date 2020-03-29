(ns lgo.core
  (:gen-class)
  (:require [lgo.board :refer :all] ;;for repl convenience
            [lgo.game :refer :all]
            [lgo.sgf :refer :all]
            [lgo.grid :refer :all]
            [lgo.gtp :refer :all]))

;; to save compile time property into a runtime one
(defmacro get-version []
  (System/getProperty "lambdago.version"))

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (println "LambdaGo"
           (get-version))
                                        ;(load-file (first args))
  (gtp-loop)
  (shutdown-agents))
