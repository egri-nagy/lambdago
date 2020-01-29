(ns lgo.core
  (:gen-class))

;; to save compile time property into a runtime one
(defmacro get-version []
  (System/getProperty "lambdago.version"))

(defn -main
  "The first argument is a name of a file containing Clojure source code.
  This main method evaluates the forms contained."
  [& args]
  (println "LambdaGo"
           (get-version))
  (load-file (first args))
  (shutdown-agents))
