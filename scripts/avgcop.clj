(require '[lgo.analysis.katago :refer [katago-output]])
(require '[lgo.analysis.processing])
(require '[lgo.util :refer [filename-without-extension]])
(require '[clojure.java.io])


(let [raws (into {} (map
                     (fn [filename]
                       [(filename-without-extension
                         (.getName (clojure.java.io/file filename)))
                        (katago-output filename)])
                     (rest args)))]
  )