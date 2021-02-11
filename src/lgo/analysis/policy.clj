(ns lgo.analysis.policy
  "Functions for working with KataGo's policy output table."
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
                                           col->code]]))

;HIGHER LEVEL ANALYSIS

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

;; HIT RATE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn hit?
  "Given the candidate moves and the raw policy, this answers the question:
  Is the best candidate the same as the policy's top move?"
  [candidates policy]
  (let [raw-best (first
                  (apply max-key
                         second ;gets the policy entry
                         (map-indexed vector policy))) ;;associating numbers to policy entries
        top (ffirst candidates)] ; cause it's sorted
    (= raw-best (policy-table-index top))))

(defn check-hits
  "Checks the whole game data for for hits."
  [outfile]
  (let [ko (katago-output outfile)
        hits (count (filter true?
                            (map (partial apply hit?)
                                 (map (juxt :candidates :policy) ko))))]
    [hits (float (/ hits (count ko)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exp-visit-count
  "The move selection mechanism in AlphaGo Zero with temperature control."
  [visitcounts tau]
  (let
      [expd (map (fn [x] (math/expt x (/ 1 tau) )) visitcounts)
       expdsum (apply + expd)]
    (map (fn [x] (float (/ x expdsum))) expd)))
