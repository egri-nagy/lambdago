(ns lgo.league
  "Function for managing a league. Calculating updates in ranking for players."
  (:require [lgo.elo :refer [rating-adjustment EA]]
            [lgo.stats :refer [mean]]))

(defn process-games
  "Batch processing game results. Games should be stored in a sequence.
  The players' database is a hash-map of names (strings) to ratings.
  Each game is recorded as a hash-map, e.g.
  {:b \"Alice\" :w \"Bob\" :r \"b+1.5\"}.
  K - the factor for Elo calculation."
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

(defn process-team-games
  "Batch processing game results played by teams, pair go, rengo."
  [players games K]
  (reduce (fn [plyrs {b :b w :w r :r}]
            (let [Rb (mean (map plyrs b))
                  Rw (mean (map plyrs w))
                  S (if ( = (first r) \b) 1.0 0.0)
                  Db (rating-adjustment S (EA Rb Rw) K)
                  rfn (fn [plyrs team delta op]
                        (reduce (fn [m x] (update-in m [x] op delta))
                                plyrs
                                team))]
              (-> plyrs
                  (rfn b Db +)
                  (rfn w Db -))))
          players
          games))


(defn print-ratings
  "Prints the names and the ratings to the console."
  [players]
  (doseq [[name rating] (reverse (sort-by  second players))]
    (println name " " rating)))

(defn even-pairings
  "Gives a pairing for the players based on their ratings for even games
  with nigiri. Missing players are ignored.
  The top player can end up as a bye, or play a simul."
  [players missing]
  (let [names (map first players)
        present (remove (set missing) names)
        ordered (sort-by players present)]
    (partition 2 ordered)))

(defn handicap-pairings
  "Cuts the players into an upper and loewr part and pairs them systematically
  by folding for handicap games."
  [players missing]
  (let [names (map first players)
        present (remove (set missing) names)
        ordered (sort-by players present)
        h (int (/ (count ordered) 2))]
    (apply map vector (partition h ordered))))
