(ns lgo.sgf
  "Functions for working with the Smart Game Format.
  Extracting positions and move sequences.
  Exporting to the (unreleased) goban LaTeX package."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

;; a crude parser for SGF files for extracting property values
;; to see the grammar, just print SGFparser, it is a bit unreadable due to
;; double escapes
(def SGFparser
  (insta/parser (str "GameRecord = GameTree+                         \n"
                     "GameTree   = <\"(\"> Node* GameTree* <\")\">   \n"
                     "Node = <\";\"> Property*                       \n"
                     "Property   = Identifier Value*                 \n"
                     "Identifier  = #'[A-Z]+'                        \n"
                     "Value  = <\"[\"> #\"(\\\\.|[^\\\\\\]]*)*\" <\"]\">")))

;; transform functions for instaparse, turning properties really into pairs,
;; other nodes just returned or grouped into a sequence
(def flatteners
  (let [f (fn [& args] (if (= 1 (count args))
                         (first args)
                         args))]
    {:Property (comp (fn [l] (mapv second l)) list)
     :GameRecord f
     :Node f
     :GameTree f}))

(defn flatten-to-vectors
  "Flattens a nested collection but stops at vectors.
  Modified version of clojure.core's flatten.
  It assumes that the tree structures is in lists, not in vectors."
  [coll]
  (filter vector?
          (rest (tree-seq (complement vector?)
                          seq
                          coll))))

(defn flat-list-properties
  "It returns a list of properties of given SGF string. A property is a vector
  starting with and identifier followed by values.
  This removes newlines before parsing the SGF, which seems to acceptable.
  The grammar is way simpler this way."
  [sgfstring]
  (let [sgf (string/join (remove #{\newline \return} sgfstring))
        pt (SGFparser sgf)
        simplified-pt (insta/transform flatteners pt)]
    (flatten-to-vectors simplified-pt)))



(defn extract-properties
  "Extracting values of properties matching a predicate function from a
  flattened parse tree of an SGF file."
  [flp p]
  (filter (comp p first) flp))

(defn extract-property [flp ID];;flattened-parse-tree
  (extract-properties flp #{ID}))

(defn extract-single-value
  "Returns one value for the given ID. It's the first value if there
  are many. Returns nil if there is no such value."
  [flp ID]
  (second (first (extract-property flp ID))))

(defn extract-game-moves
  [sgf]
  (extract-properties (flat-list-properties sgf)
                      #(or (= % "B") (= % "W"))))

(defn game-data
  "Returns a hash-map containing game information extracted from
  the SGF string."
  [sgf]
  (let [flp (flat-list-properties sgf)]
    {:rules (or (extract-single-value flp "RU") "japanese")
     :black (or (extract-single-value flp "PB") "BLACK")
     :white (or (extract-single-value flp "PW") "WHITE")
     :result (or (extract-single-value flp "RE") "unknown")
     :komi (read-string (extract-single-value flp "KM"))
     :size (read-string (extract-single-value flp "SZ"))
     :moves (extract-properties flp #(or (= % "B") (= % "W")))}))

;; LaTeX export to the goban package
(defn positionsgf->goban
  "Converts SGF board positions to goban (LaTeX) format."
  [sgf]
  (let [fpt (flat-list-properties sgf)
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
(def points->GTPcoords (zipmap (range 1 27) "ABCDEFGHJKLMNOPQRST"))

(defn SGFcoord->GTPcoord
  [[col row] size]
  (str (points->GTPcoords (SGFcoords->points col))
       (- (inc size) (SGFcoords->points row))))


(defn SGF-game-moves->lgo
  [SGF-moves]
  (map (fn [p] [({"B" :b "W" :w} (first p)) (mapv SGFcoords->points (second p))])
       SGF-moves))

(defn lgo->SGF
  [size lgomoves]
  (str
   "(;GM[1]FF[4]CA[UTF-8]AP[LambdaGo]KM[0]SZ["
   size "]DT[2020-03-20]"
   (string/join
    (map (fn [x y]
           (str ";" ({:b "B" :w "W"} x)
                "["
                (points->SGFcoords (y 0)) (points->SGFcoords (y 1))
                "]"))
         (cycle [:b :w])
         lgomoves))
   ")"))


(defn filename
  "Cuts the extension of the filename."
  [sgf_file]
  (string/join (butlast (string/split sgf_file #"\."))))

;;

;;(sgf/SGF-game-moves->lgo (sgf/extract-game-moves (slurp "example.sgf")))
