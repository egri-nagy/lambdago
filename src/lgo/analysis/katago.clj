(ns lgo.analysis.katago
  "Functions for doing KataGo analysis directly.
  Preparing input files for the analysis engine and parsing the
  output into a map suitable for visualization."
  (:require [clojure.data.json :as json]
            [clojure.string :refer [lower-case
                                    join
                                    split]]
            [clojure.math.numeric-tower :as math]
            [clojure.java.io :as io]
            [lgo.stats :refer [normalize
                               KL-divergence
                               median
                               mean]]
            [lgo.analysis.converters :refer [B<->W
                                             code->col
                                             black<->white
                                             col->code]]
            [lgo.sgf :refer [game-data
                             filename
                             SGFcoord->GTPcoord]]))

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
                       [col (SGFcoord->GTPcoord move)]))
                   (:moves gd))
        col (ffirst moves)
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
        (update :includePolicy (constantly false))
        (update :id (constantly (str "passed " code)))
        (update :maxVisits (constantly maxvisits)))))
   kid))

(defn process-sgf
  [sgf_file max-visits passed-max-visits]
  (let [name (filename sgf_file)
        output (str name ".in")
        kgd (map #(conj % [:maxVisits max-visits])
                 (katago-input-data (slurp sgf_file)))]
    (spit output
          (join "\n"
                (map json/write-str
                     (concat kgd
                             (katago-passed-game-data kgd
                                                      passed-max-visits)))))))

;; PROCESSING THE OUTPUT  of the analysis engine.

(defn katago-turn-data
  "Processing one line of the KataGo output."
  [js]
  (let [d (json/read-str js :key-fn keyword)
        means (map :scoreMean (sort-by :order (:moveInfos d))) ;sorting to make sure
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
       d (map katago-turn-data (line-seq rdr))
       cats (group-by :category d)]
    (into {} (map (fn [[k v]] [(keyword k) (sort-by :move v)])
                  cats))))

;;HIGHER LEVEL ANALYSIS

;;policy comparison
(defn policy-table-index
  "Converting a GTP? move to a policy table index."
  [move]
  (let [m (zipmap "ABCDEFGHJKLMNOPQRST" (range))
        letter (first move)
        num (read-string (join (rest move)))]
    (if (= "pass" move)
      361
      (+ (m letter) (* 19 (- 19 num))))))

(defn policy-comparison
  "Compares the earlier policy P with the later policy Q.
  It takes the top N moves from policy Q, finds the corresponding policy
  values from P. Assuming that these policy values are all positive, we
  normalize them, then calculate the KL-divergence."
  [candidates policy]
  (let [cands (filter (fn [[_ visits]] (pos? 0)) ; shall we do this or not?
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
        top (ffirst candidates)] ; cause it's sorted
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
