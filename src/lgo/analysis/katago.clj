(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  "
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [lgo.stats :refer [normalize KL-divergence]]
            [lgo.sgf :refer [extract-game-moves
                             SGFcoord->GTPcoord]]))

(defn game-moves
  [sgf maxvisits]
  (let [moves (map (fn [[col move]] [col (SGFcoord->GTPcoord move)])
                   (extract-game-moves sgf))]
    (json/write-str
     {:id "foo"
      :rules "japanese"
      :komi 6
      :intitialPlayer "black"
      :boardXSize 19
      :boardYSize 19
      :analyzeTurns (range (inc (count moves)))
      :maxVisits maxvisits
      :moves moves
      :includePolicy true})))


(defn turn-data
  [js]
  (let [d (json/read-str js :key-fn keyword)]
    {:move (:turnNumber d)
     :mean (:scoreLead (:rootInfo d))
     :means (map (juxt :scoreMean :order) (:moveInfos d))} ))

(defn read-json
  [filename]
  (let
   [rdr (clojure.java.io/reader filename)]
    (map turn-data (line-seq rdr))))

(defn policy-comparison
  "Compares the earlier policy P with the later policy Q.
  It takes the top N moves from policy Q, finds the corresponding policy
  values from P. Assuming that these policy values are all positive, we
  normalize them, then calculate the KL-divergence."
  [P Q N]
  (let [indexedQ (map vector Q (range))
        sortedQ (sort-by first > indexedQ)
        topN (take N sortedQ)
        topNindices (map second topN)
        vP (vec P)
        Pdist (normalize (map vP topNindices))
        Qdist (normalize(map first topN))]
    (KL-divergence Pdist Qdist)))
