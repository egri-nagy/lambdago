(ns lgo.elo
  "Function for calculating Elo points."
  (:require [clojure.math.numeric-tower :as math]))

(defn Q
  "Just a pre-calculation of the player's rating,
  used in the expectation formula."
  [R]
  (math/expt 10 (/ R 400)))

(defn EA
  "Expected result for player against player B. The chance of winning."
  [RA RB]
  (let [QA (Q RA)
        QB (Q RB)]
    (/ QA (+ QA QB))))

(defn rating-adjustment
  "The rating adjustment for result SA and expectation EA with constant K."
  [SA EA K]
  (math/round (* K (- SA EA))))

(defn new-rating
  "Calculates the new rating for player A."
  [RA RB result K]
  (+ RA (rating-adjustment result (EA RA RB) K)))

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
    (println name " " rating))
)
