(ns lgo.sgf
  "Functions for working with the Smart Game Format"
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

;; a crude parser for SGF files, mainly for extracting board positions and move
;; sequences
(def SGFparser (insta/parser (slurp "resources/SGF.bnf")))

;; transform function for instaparse, turning properties really into pairs
(def flatten-properties
  {:Property (comp (fn [l] (mapv second l)) list)})

(defn extract-property [fpt ID];;flattened-parse-tree
  (filter #(and (vector? %) (= ID (first %)))
          (tree-seq vector? identity fpt)))

(defn positionsgf->goban
  "Converts SGF board positions to goban (LaTeX) format."
  [sgf]
  (let [pt (SGFparser sgf)
        fpt (insta/transform flatten-properties pt)
        size (second (first (extract-property fpt "SZ")))
        white-stones (string/join ","
                                  (rest (first (extract-property fpt "AW"))))
        black-stones (string/join ","
                                  (rest (first (extract-property fpt "AB"))))]
    (str "\\gobanclear"
         "\\gobansize{" size "}{" size "}\n"
         "\\gobanplace{black}{" black-stones "}\n"
         " \\gobanplace{white}{" white-stones "}\n"
         "\\gobanshowfull")))
