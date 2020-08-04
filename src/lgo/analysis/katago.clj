(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  "
  (:require [clojure.string :as string]
            [lgo.sgf :refer [extract-game-moves]]))
