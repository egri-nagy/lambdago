(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  Preparing input files for the analysis engine and parsing the
  output into a map suitable for visualization."
  (:require [clojure.data.json :as json]
            [clojure.string :refer [lower-case
                                    join
                                    split]]
            [clojure.java.io :as io]
            [lgo.stats :refer [mean
                               median]]
            [lgo.analysis.converters :refer [code->col
                                             black<->white
                                             col->code]]
            [lgo.sgf :refer [game-data
                             SGFcoord->GTPcoord]]
            [lgo.util :refer [filename-without-extension]]))

;; GENERATING INPUT files for the KataGo Analysis Engine ;;;;;;;;;;;;;;;;;;;;;;

(defn prefixes
  "All prefixes of the given collection, starting from the empty to
  the complete collection. Returns a sequence containing the prefixes
  as vectors."
  [coll]
  (reductions conj [] coll))

(defn katago-input-data
  "Produces maps containing input data extracted from a game
  for the KataGo analysis engine."
  [sgf]
  (let [gd (game-data sgf)
        moves (map (fn [[col move]]
                     (if (empty? move)
                       [col "pass"]
                       [col (SGFcoord->GTPcoord move (:size gd))]))
                   (:moves gd))
        col (ffirst moves)
        initial-player (code->col col)]
    (for [mvs (prefixes moves)] ;katago needs the previous moves
      (let [player (if (even? (count mvs))
                     initial-player
                     (black<->white initial-player))]
        {:id (str "game" " "  (col->code player)) ;the id tells whether it is a game move or not
         :rules (lower-case (:rules gd))
         :komi (:komi gd)
         :initialPlayer initial-player
         :boardXSize (:size gd)
         :boardYSize (:size gd)
         :moves mvs
         :includePolicy true}))))

(defn katago-passed-game-data
  "Takes already prepared katago input data, and appends a pass."
  [kid maxvisits]
  (map
   (fn [d]
     (let [code (second (split (:id d) #" "))]
       (->
        d
        (update :moves #(conj % [code "pass"]))
        ;;no policy details needed for imaginary pass
        (update :includePolicy (constantly false)) ;we don't want the policy table for the passed
        (update :id (constantly (str "passed " code)))
        (update :maxVisits (constantly maxvisits)))))
   kid))

(defn process-sgf
  ([sgf_file max-visits]
   (process-sgf sgf_file max-visits 0))
  ([sgf_file max-visits passed-max-visits]
   (let [name (filename-without-extension sgf_file)
         output (str name ".in")
         kgd (map #(conj % [:maxVisits max-visits])
                  (katago-input-data (slurp sgf_file)))
         passed (when-not (zero? passed-max-visits)
                  (katago-passed-game-data kgd passed-max-visits))]
     (spit output
           (join "\n"
                 (map json/write-str
                      (concat kgd passed)))))))

;; PROCESSING THE OUTPUT  of the analysis engine.

(defn katago-turn-data
  "Processing one line of the KataGo output. It also adds some statistical calculations.
  Outputs a hash-map."
  [js]
  (let [d (json/read-str js :key-fn keyword) ;the result of parsing JSON
        means (map :scoreMean (sort-by :order (:moveInfos d))) ;sorting to make sure
        omv (map (juxt :order :move :visits) (:moveInfos d))
        candidates (map (comp vec rest) (sort-by first omv)) ;stripping the order number
        [category color] (split (:id d) #" ")
        move (:turnNumber d)]
    {:move move
     :color (code->col color)
     :winrate (:winrate (:rootInfo d))
     :candidates candidates ;candidate moves and their visit counts in the order of strength
     :policy (:policy d)
     :mean (:scoreLead (:rootInfo d))
     :means means
     :meanmean (mean means)
     :medianmean (median means)
     :category category}))

(defn katago-output
  "Processing a whole game analyzed by KataGo, proceeding line-by-line.
  The input parameter is the filename of the KataGo output file."
  [filename]
  (let ;todo: what's wrong with with-open?
      [rdr (io/reader filename)
       d (map katago-turn-data (line-seq rdr))
       cats (group-by :category d)]
    (into {}
          (map (fn [[k v]] [(keyword k) (sort-by :move v)]) ;sorting by move
               cats))))
