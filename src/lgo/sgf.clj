(ns lgo.sgf
  "Functions for working with the Smart Game Format.
  https://www.red-bean.com/sgf/
  Extracting positions and move sequences.
  Exporting to the (unreleased) goban LaTeX package."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]))

;; parsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a parser for SGF files
;; to see the grammar, just print SGFparser, it is a bit unreadable due to
;; double escapes
(def SGFparser
  (insta/parser (str "GameTree   = <\"(\"> Node* GameTree* <\")\">   \n"
                     "Node = <\";\"> Property*                       \n"
                     "Property   = Identifier Value*                 \n"
                     "Identifier  = #'[A-Z]+'                        \n"
                     "Value  = <\"[\"> #\"(\\\\.|[^\\\\\\]]*)*\" <\"]\">")))

(defn prepare-sgf
  "Removes newlines from the sgf string in preparation for parsing.
  This seems to acceptable. The grammar is way simpler this way"
  [sgfstring]
  (string/join
   (remove #{\newline \return} sgfstring)))

;; functions working on the parsed tree ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn remove-variations
  "Removes variations, keeping only the main line of the game, assuming it is
  the first variation."
  [pt]
  (into [:GameTree] ;need to put his one back for the main variation
        (insta/transform
         {:GameTree (fn [& children]
                      (let [node?  #(= :Node (first %))
                            nodes (take-while node? children)
                            tree (first (drop-while node? children))]
                        (vec (concat nodes tree))))}
         pt)))

(defn pt2sgf
  "Reassembles the parse tree into the corresponding SGF string."
  [pt]
  (insta/transform
   {:GameTree (fn [& children] (str "(" (apply str children) ")"))
    :Node (fn [& children] (apply str ";" children))
    :Identifier identity
    :Value (fn [v] (str "[" v "]"))
    :Property str}
   pt))

(defn remove-properties
  "Removes all properties with the given identifiers.
  It is meant for comments, and analysis info. Removing moves would break things?"
  [pt idset]
  (insta/transform
   {:Node (fn [& children]
            ;;recreating the node with the matching properties removed
            (into [:Node]
                  (remove
                   (fn [[_ [_ ID]] & _] ;funky destructuring to check the id
                     (idset ID))
                   children)))}
   pt))

(defn remove-property
  "Removing just one property from the parse tree. See remove-properties."
  [pt id]
  (remove-properties pt #{id}))

;;property types that we remove when extracting game data
(def not-needed-for-basic-game-data #{"C" "MA" "TR" "CR" "SQ" "LB"})

(defn clean
  "Removing commonly used unnecessary properties form the parse tree."
  [pt]
  (remove-properties pt not-needed-for-basic-game-data))

(defn simplify
  "Removes variations and unused properties from parse tree."
  [pt]
  (clean (remove-variations pt)))

(defn properties
  "All properties, identifier-value pairs in a sequence that keeps the
  order of occurence (meaningful when there are no variations)."
  [pt]
  (insta/transform
   {:Property (fn [[_ ID] [_ VALUE] & _] ;TODO: this loses multi-valued properties
                (vector ID VALUE))
    :GameTree concat
    :Node (fn [& args] (into [] args))}
   pt))


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

(defn simplified-parse-tree
  [sgfstring]
  (let [pt (SGFparser(prepare-sgf sgfstring))]
    (simplify pt)))

(defn simplified-sgf-string
  "Parses an SGF string, simplifies and returns an SGF string without variations and
  non-essential properties."
  [sgfstring]
  (pt2sgf (simplified-parse-tree sgfstring)))

(defn game-data
  "Returns a hash-map containing game information extracted from
  the SGF string. Defaults: black player BLACK, white player WHITE, rule set japanese,
  komi 6.5."
  [sgfstring]
  (let [flp (properties (simplified-parse-tree sgfstring))]
    {:rules (or (extract-single-value flp "RU") "japanese")
     :black (or (extract-single-value flp "PB") "BLACK")
     :white (or (extract-single-value flp "PW") "WHITE")
     :result (or (extract-single-value flp "RE") "unknown")
     :komi (let [komi  (extract-single-value flp "KM")]
             (if komi
               (read-string komi)
               6.5))
     :size (read-string (or (extract-single-value flp "SZ") "19"))
     :moves (extract-properties flp #(or (= % "B") (= % "W")))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX export to the goban package
(defn positionsgf->goban
  "Converts SGF board positions to goban (LaTeX) format."
  [sgf]
  (let [fpt (properties sgf)
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
