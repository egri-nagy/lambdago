(ns lgo.analysis.policy
  "Functions for working with KataGo's policy output table."
(:require [clojure.string :refer [join]]
          [clojure.math.numeric-tower :as math]
          [lgo.stats :refer [normalize
                             KL-divergence]]))

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

(defn GTP-move
  "Converting a policy table index to GTP move."
  [index]
  (if (= 361 index) "pass"
      (let [row (math/floor (/ index 19))
            column (mod index 19)
            m (zipmap (range) "ABCDEFGHJKLMNOPQRST") ]
        (str (m  column) (- 19 row) ))))

(defn policy-comparison
  "Compares the earlier policy P with the later policy Q.
  It takes the top N moves from policy Q, finds the corresponding policy
  values from P. Assuming that these policy values are all positive, we
  normalize them, then calculate the KL-divergence."
  [candidates policy]
  (let [cands (filter (fn [[_ visits]] (pos? visits)) ; shall we do this or not?
                      candidates)
        P (normalize (map second cands))
        PI (normalize (map (fn [move] (nth policy (policy-table-index move)))
                           (map first cands)))]
    (KL-divergence P PI)))

(defn check-updated-policy
  "Batch policy comparison."
  [kgdat]
  (map (partial apply policy-comparison)
       (map (juxt :candidates :policy) kgdat)))

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
  [kgdat]
  (let [hits (count (filter true?
                            (map (partial apply hit?)
                                 (map (juxt :candidates :policy) kgdat))))]
    [hits (float (/ hits (count kgdat)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn exp-visit-count
  "The move selection mechanism in AlphaGo Zero with temperature control."
  [visitcounts tau]
  (let
      [expd (map (fn [x] (math/expt x (/ 1 tau) )) visitcounts)
       expdsum (apply + expd)]
    (map (fn [x] (float (/ x expdsum))) expd)))
