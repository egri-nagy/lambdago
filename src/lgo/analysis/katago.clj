(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  "
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [lgo.stats :refer [normalize KL-divergence median mean]]
            [lgo.sgf :refer [extract-game-moves
                             SGFcoord->GTPcoord
                             flat-list-properties
                             extract-single-value]]))

(defn game-moves
  [sgf maxvisits]
  (let [moves (map (fn [[col move]] [col (SGFcoord->GTPcoord move)])
                   (extract-game-moves sgf))
        flp (flat-list-properties sgf)
        m {"B" "black", "W" "white"}]
    (json/write-str
     {:id "foo"
      :rules (extract-single-value flp "RU")
      :komi (read-string (extract-single-value flp "KM"))
      :initialPlayer "black"
      :boardXSize (read-string (extract-single-value flp "SZ"))
      :boardYSize (read-string (extract-single-value flp "SZ"))
      :analyzeTurns (range (inc (count moves)))
      :maxVisits maxvisits
      :moves moves
      :includePolicy true})))


(defn turn-data
  [js]
  (let [d (json/read-str js :key-fn keyword)
        means (map :scoreMean (:moveInfos d))]
    {:move (:turnNumber d)
     :mean (:scoreLead (:rootInfo d))
     :means means
     :meanmean (mean means)
     :medianmean (median means)} ))

(defn read-json
  [filename]
  (let ;todo: what's wrong with with-open?
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
