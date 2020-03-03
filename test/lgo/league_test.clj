(ns lgo.league-test
  (:require [clojure.test :refer :all]
            [lgo.league :refer :all]))

(def players
  {"A" 1200
   "B" 1300
   "C" 1400
   "D" 1500})

(def pair-go-games
  [{:b ["A" "C"] :w ["B" "D"] :r "b+1.5"}])

(deftest team-games-test
  (testing "Testing team games."
    (is (= (process-team-games players pair-go-games 32)
           {"A" 1220, "B" 1280, "C" 1420, "D" 1480} ))))
