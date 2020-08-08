(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  "
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
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
