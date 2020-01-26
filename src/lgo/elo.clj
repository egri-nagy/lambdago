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

(defn rating-adjustment [SA EA K]
  (math/round (* K (- SA EA))))

(defn new-rating [RA RB result K]
  (+ RA (rating-adjustment result (EA RA RB) K)))

(defn process-games [players games K]
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

(def players
  {"A" 1200
   "B" 1200})

(def games
  [
   {:b "A" :w "B" :r "b+12.5"}
   {:b "A" :w "B" :r "w+1.5"}
   {:b "A" :w "B" :r "b+r"}
   {:b "A" :w "B" :r "b+r"}
   {:b "A" :w "B" :r "w+2.5"}
   ])


(def t (process-games players games 32))

(doseq [[name rating] (reverse (sort-by  second t ))]
  (println name " " rating))
