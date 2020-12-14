(def DIR "FOLDER_TO_BE_SCANNED_FOR_SGFS")
(require '[clojure.java.io :as io :refer :all])
 (require '[lgo.sgf :refer :all])
(def fs (.list (io/file DIR)))
(doseq [f (sort fs)]
  (println "%" f)
  (println (positionsgf->goban (slurp (str DIR f)))))
