(ns lgo.sgf
  "Functions for working with the Smart Game Format.
  Extracting positions and move sequences.
  Exporting to the (unreleased) goban LaTeX package."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.matrix.stats :refer [mean sd]]))

;; a crude parser for SGF files for extracting property values
(def SGFparser
  (insta/parser (str "GameRecord = GameTree+                         \n"
                     "GameTree   = <\"(\"> Node* GameTree* <\")\">   \n"
                     "Node = <\";\"> Property*                       \n"
                     "Property   = Identifier Value*                 \n"
                     "Identifier  = #'[A-Z]+'                        \n"
                     "Value  = <\"[\"> #\"([^\\\\\\]]|\\\\.)*\" <\"]\">")))

;; transform function for instaparse, turning properties really into pairs
(def flatten-properties
  {:Property (comp (fn [l] (mapv second l)) list)})

(defn extract-property [fpt ID];;flattened-parse-tree
  (filter #(and (vector? %) (= ID (first %)))
          (tree-seq vector? identity fpt)))

(defn flattened-parse-tree
  "It returns the flattened parse tree ready for property value extraction of a
  given SGF string.
  It has to remove returns and newlines (or can we modify the grammar?)"
  [sgfstring]
  (let [sgf (string/join (remove #{\newline \return} sgfstring))
        pt (SGFparser sgf)
        fpt (insta/transform flatten-properties pt)]
    fpt))

(defn extract-properties
  "Extracting values of properties matching a predicate function from a
  flattened parse tree of an SGF file."
  [fpt p]
  (filter #(and (vector? %) (p (first %)))
          (tree-seq vector? identity fpt)))

(defn extract-game-moves
  [sgf]
  (extract-properties (flattened-parse-tree sgf)
                      #(or (= % "B") (= % "W"))))

;; LaTeX export to the goban package
(defn positionsgf->goban
  "Converts SGF board positions to goban (LaTeX) format."
  [sgf]
  (let [fpt (flattened-parse-tree sgf)
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

(def SGFcoords->points (zipmap "abcdefghijklmnopqrstuvwxyz" (range 1 27)))
(def points->SGFcoords (zipmap (range 1 27) "abcdefghijklmnopqrstuvwxyz"))

(defn SGF-game-moves->lgo
  [SGF-moves]
  (map (fn [p] [({"B" :b "W" :w} (first p)) (mapv SGFcoords->points (second p))])
       SGF-moves))

(defn lgo->SGF
  [size lgomoves]
  (str
   "(;GM[1]FF[4]CA[UTF-8]AP[LambdaGo]KM[0]SZ["
   size "]DT[2020-03-20]"
   (apply str  (map (fn [x y]
                      (str ";" ({:b "B" :w "W"} x)
                           "["
                           (points->SGFcoords (y 0)) (points->SGFcoords (y 1))
                           "]"))
                    (cycle [:b :w])
                    lgomoves))
   ")"))

;;

;;(sgf/SGF-game-moves->lgo (sgf/extract-game-moves (slurp "example.sgf")))

;; (require '[clojure.java.io :as io :refer :all])
;;  (require '[lgo.sgf :refer :all])
;;  (def fs (.list (io/file "/home/dersu/igomath/PROBLEMS/IGOPUZZLE/")))
;; (doseq [f (sort fs)] (println "%" f) (println (positionsgf->goban (slurp (str "/home/dersu/igomath/PROBLEMS/IGOPUZZLE/" f)))))
