(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  "
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [lgo.sgf :refer [extract-game-moves
                             SGFcoord->GTPcoord]]))

(defn game-moves
  [sgf]
  (json/write-str
   {:id "foo"
    :rules "japanese"
    :komi 6
    :boardXSize 19
    :boardYSize 19
    :analyzeturns [100]
    :maxvisits 20000
    :moves
    (map (fn [[col move]] [col (SGFcoord->GTPcoord move)])
         (extract-game-moves sgf))}))
