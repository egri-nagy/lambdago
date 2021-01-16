(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  Preparing input files for the analysis engine directly."
  (:require [clojure.data.json :as json]
            [clojure.string :refer [lower-case join split]]
            [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io]
            [lgo.stats :refer [normalize KL-divergence median mean]]
            [lgo.analysis.converters :refer [B<->W code->col black<->white col->code]]
            [lgo.sgf :refer [game-data
                             filename
                             SGFcoord->GTPcoord]]))

;; Generating input files for the KataGo Analysis Engine

(defn katago-game-data
  "Produces a JSON string containing input data extracted from a game
  for the KataGo analysis engine."
  [sgf]
  (let [gd (game-data sgf)
        moves (map (fn [[col move]]
                     (if (empty? move)
                       [col "pass"]
                       [col (SGFcoord->GTPcoord move)]))
                   (:moves gd))
        code (first (first moves))
        first-player (code->col code)]
    {:id code ; a hack to put the first player in id, we analyze only one game
     :rules (lower-case (:rules gd))
     :komi (:komi gd)
     :initialPlayer first-player
     :boardXSize (:size gd)
     :boardYSize (:size gd)
     :moves moves
     :includePolicy true}))

(defn katago-input-all-moves
  "Produces a JSON input file for the KataGo Analysis Engine
  for analyzing all the moves of the game."
  [sgf maxvisits]
  (let [kgd (katago-game-data sgf)]
    (json/write-str
     (conj kgd
           [:maxVisits maxvisits]
           [:analyzeTurns (range (inc (count(:moves kgd))))]))))

(defn katago-input-random-move
  "Produces a JSON input file for the KataGo Analysis Engine
  for analyzing a single random move of the game."
  [sgf maxvisits]
  (let [kdg (katago-game-data sgf)]
    (json/write-str
     (conj kdg
           [:maxVisits maxvisits]
           [:analyzeTurns [(rand-int (inc (count (:moves kdg))))]]))))

(defn katago-input-given-moves
  "Produces a JSON input file for the KataGo Analysis Engine
  for analyzing the given moves of the game."
  [sgf maxvisits moves]
  (let [kdg (katago-game-data sgf)]
    (json/write-str
     (conj kdg
           [:maxVisits maxvisits]
           [:analyzeTurns moves]))))

(defn process-sgf
  [sgf_file]
  (let [name (filename sgf_file)
        output (str name ".in")]
    (println output)
    (spit output (katago-input-all-moves (slurp sgf_file) 100000))))

;; doing the moves separately

(defn prefixes
  "All prefixes of the given collection, starting from the empty to
  the complete collection. Returns a sequence containing the prefixes
  as vectors."
  [coll]
  (reductions conj [] coll))

(defn katago-game-data2
  "Produces a JSON string containing input data extracted from a game
  for the KataGo analysis engine."
  [sgf]
  (let [gd (game-data sgf)
        moves (map (fn [[col move]]
                     (if (empty? move)
                       [col "pass"]
                       [col (SGFcoord->GTPcoord move)]))
                   (:moves gd))
        col (first (first moves))
        initial-player (code->col col)]
    (for [mvs (prefixes moves)]
      (let [player (if (even? (count mvs))
                     initial-player
                     (black<->white initial-player))]
        {:id (str "game" " "  (col->code player))
         :rules (lower-case (:rules gd))
         :komi (:komi gd)
         :initialPlayer initial-player
         :boardXSize (:size gd)
         :boardYSize (:size gd)
         :moves mvs
         :includePolicy true
         :maxvisits 10000}))))

(defn katago-passed-game-data
  [kgd]
  (map
   (fn [d]
     (let [code (second (split (:id d) #" "))]
       (->
        d
        (update :moves #(conj % [code "pass"]))
        (update :includePolicy (constantly false))
        (update :id (constantly (str "passed " code))))))
   kgd))

(defn process-sgf2
  [sgf_file]
  (let [name (filename sgf_file)
        output (str name ".in")
        kgd (katago-game-data2 (slurp sgf_file))]
    (spit output
          (join "\n"
                (map json/write-str
                     (concat kgd
                             (katago-passed-game-data kgd)))))))

;; Processing the output  of the analysis engine.

(defn katago-turn-data
  "Processing one line of the KataGo output."
  [js]
  (let [d (json/read-str js :key-fn keyword)
        means (map :scoreMean (:moveInfos d))
        omv (map (juxt :order :move :visits) (:moveInfos d))
        candidates (map (comp vec rest) (sort-by first omv))
        [category color] (split (:id d) #" ")
        move (:turnNumber d)]
    {:move move
     :color color
     :winrate (:winrate (:rootInfo d))
     :candidates candidates
     :policy (:policy d)
     :mean (:scoreLead (:rootInfo d))
     :means means
     :meanmean (mean means)
     :medianmean (median means)
     :category category} ))

(defn katago-output
  "Processing a whole game analyzed by KataGo."
  [filename]
  (let ;todo: what's wrong with with-open?
      [rdr (io/reader filename)
       d (map katago-turn-data (line-seq rdr))]
    (sort-by :move d)))

;;higher level analysis
;;policy comparison

(defn policy-table-index
  "Converting a GTP? move to a policy table index."
  [move]
  (let [m (zipmap "ABCDEFGHJKLMNOPQRST" (range))
        letter (first move)
        num (read-string (apply str (rest move)))]
    (if (= "pass" move)
      361
      (+ (m letter) (* 19 (- 19 num))))))

(defn policy-comparison
  "Compares the earlier policy P with the later policy Q.
  It takes the top N moves from policy Q, finds the corresponding policy
  values from P. Assuming that these policy values are all positive, we
  normalize them, then calculate the KL-divergence."
  [candidates policy]
  (let [cands (filter (fn [[_ visits]] (> visits 0)) ; shall we do this or not?
                      candidates)
        P (normalize (map second cands))
        PI (normalize (map (fn [move] (nth policy (policy-table-index move)))
                           (map first cands)))]
    (KL-divergence P PI)))

(defn check-updated-policy
  "Batch policy comparison."
  [outfile]
  (let [ko (katago-output outfile)]
    (map (partial apply policy-comparison)
         (map (juxt :candidates :policy) ko))))

;; (def b40 (apply concat (map check-updated-policy (filter (fn [f] (string/ends-with? (.getName f) ".out")) (file-seq (clojure.java.io/file "/media/dersu/PRINT/b40/"))))))

;;hitrate

(defn hit?
  [candidates policy]
  (let [raw-best (first (apply max-key second (map-indexed vector policy)))
        top (first (first candidates))] ; cause it's sorted
    (= raw-best (policy-table-index top))))

(defn check-hits
  [outfile]
  (let [ko (katago-output outfile)
        hits (count (filter true?
                            (map (partial apply hit?)
                                 (map (juxt :candidates :policy) ko))))]
    [hits (float (/ hits (count ko)))]))

(defn exp-visit-count
  "The move selection mechanism in AlphaGo Zero with temperature control."
  [visitcounts tau]
  (let
      [expd (map (fn [x] (math/expt x (/ 1 tau) )) visitcounts)
       expdsum (apply + expd)]
    (map (fn [x] (float (/ x expdsum))) expd)))
