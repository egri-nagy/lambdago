(ns lgo.sgf
  "Functions for working with the Smart Game Format"
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

(def SGFparser (insta/parser (slurp "resources/SGF.bnf")))

(def flatten
  {:Property (comp (fn [l] (mapv second l)) list)})

(defn extract-property [fpt ID];;flattened-parse-tree
  (filter #(and (vector? %) (= ID (first %)))
          (tree-seq vector? identity fpt)))

(defn extract-position
  [sgf]
  (let [pt (SGFparser sgf)
        fpt (insta/transform flatten pt)
        whites (rest (first (extract-property fpt "AW")))
        blacks (rest (first (extract-property fpt "AB")))]
    {:white whites :black blacks}))

(defn position->goban
  [m]
  (let [bs (string/join "," (:black m))
        ws (string/join "," (:white m))]
    (str "\\gobanplace{black}{" bs "} \\gobanplace{white}{" ws "}")))
