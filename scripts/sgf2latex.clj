(def DIR "/home/dersu/igomath/PROBLEMS/7x7/right_side_to_atari/")
(require '[clojure.java.io :as io :refer :all])
 (require '[lgo.sgf :refer :all])
(def fs (.list (io/file DIR)))
(doseq [f (sort fs)]
  (println "%" f)
  (println (positionsgf->goban (slurp (str DIR f)))))
