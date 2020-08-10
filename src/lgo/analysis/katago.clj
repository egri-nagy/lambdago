(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  "
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.string :refer [lower-case]]
            [lgo.stats :refer [normalize KL-divergence median mean]]
            [lgo.sgf :refer [game-data
                             SGFcoord->GTPcoord]]))

(defn katago-input
  "Produces a JSON string containing input for the KataGo analysis engine."
  [sgf maxvisits]
  (let [gd (game-data sgf)
        moves (map (fn [[col move]] [col (SGFcoord->GTPcoord move)])
                   (:moves gd))
        m {"B" "black", "W" "white"}
        first-player (m (first (first moves)))]
    (json/write-str
     {:id first-player ; a hack to put the  first player in id
      :rules (lower-case (:rules gd))
      :komi (:komi gd)
      :initialPlayer first-player
      :boardXSize (:size gd)
      :boardYSize (:size gd)
      :analyzeTurns (range (inc (count moves)))
      :maxVisits maxvisits
      :moves moves
      :includePolicy true})))

(defn katago-turn-data
  [js]
  (let [d (json/read-str js :key-fn keyword)
        means (map :scoreMean (:moveInfos d))
        first-player (:id d) ;0th
        B<->W {"B" "W", "W" "B"}
        move (:turnNumber d)]
    {:move move
     :color (if (even? move)
              first-player
              (B<->W first-player))
     :mean (:scoreLead (:rootInfo d))
     :means means
     :meanmean (mean means)
     :medianmean (median means)} ))

(defn katago-output
  [filename]
  (let ;todo: what's wrong with with-open?
   [rdr (clojure.java.io/reader filename)]
    (map katago-turn-data (line-seq rdr))))

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
