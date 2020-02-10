(ns lgo.elo
  "Function for managing a league."
  (:require [lgo.elo :refer [rating-adjustment EA]]))

(defn process-games
  "Batch processing game results."
  [players games K]
  (reduce (fn [plyrs {b :b w :w r :r}]
            (let [Rb (plyrs b)
                  Rw (plyrs w)
                  S (if ( = (first r) \b) 1.0 0.0)
                  Db (rating-adjustment S (EA Rb Rw) K)]
              (-> plyrs
                  (update-in [b] + Db)
                  (update-in [w] - Db))))
          players
          games))

(defn print-ratings
  "Prints the names and the ratings to the console."
  [players]
  (doseq [[name rating] (reverse (sort-by  second players))]
    (println name " " rating)))
