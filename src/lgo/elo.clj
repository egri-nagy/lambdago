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
