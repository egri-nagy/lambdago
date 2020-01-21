(ns lgo.elo
  "Function for calculating Elo points."
  (:require [clojure.math.numeric-tower :as math]))

(def K 32)

(defn Q [R]
  (math/expt 10 (/ R 400)))

(defn EA [RA RB]
  (let [QA (Q RA)
        QB (Q RB)]
    (/ QA (+ QA QB))))

(defn rating-adjustment [SA EA]
  (math/round (* K (- SA EA))))

(defn new-rating [RA RB result]
  (+ RA (rating-adjustment result (EA RA RB))))
