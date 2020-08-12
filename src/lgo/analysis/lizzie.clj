(ns lgo.analysis.lizzie
  "Functions for working with the output of Lizzie after doing KataGo analysis.

  Design decisions:
  Internally all score values are from Black's perspective: positive means Black
  win, negative means White win.
  Move counter tells how many moves were made. Color tells whose turn is it.

  The raw data is a hash-map with keys color, move, mean, meanmean, medianmean,
  means.
  "
  (:require [clojure.string :as string]
            [lgo.sgf :refer [flat-list-properties
                             extract-properties
                             extract-single-value]]
            [lgo.stats :refer [median mean]]
            [lgo.analysis.oz :refer [game-report]]))

(def B<->W {"B" "W", "W" "B"})

(defn extract-from-LZ
  "Simply extracts from LZ string s the values after the tag.
  If the tag has more values (like PV), this gets only the first."
  [s tag]
  (map second
       (filter #(= tag (first %))
               (partition 2 1
                          (clojure.string/split s #" ")))))

(defn raw-data
  "Extracts the score means and the color of the previous move from
  Katago-Lizzie output."
  [flp]
  (let [props (extract-properties flp #{"B" "W" "LZ"})
        x (map (fn [[id val]]
                 (if (#{"B" "W"} id)
                   id
                   (vector (mapv read-string
                                 (extract-from-LZ val "scoreMean"))
                           (mapv (comp #(/ % 100.0) read-string)
                                 (extract-from-LZ val "winrate")))))
               props)
        y (partition 2 x)] ;combining move and score mean
    (map (fn [[player [means winrates]] move]
           (let [meanz (if (= player "W")  ;all values form Black's perspective
                         means
                         (map (partial * -1) means))
                 wr (if (= player "W")
                      (first winrates)
                      (- 100 (first winrates)))]
             {:move move
              :color (B<->W player)
              :mean (first meanz)
              :meanmean (mean meanz)
              :medianmean (median meanz)
              :means meanz
              :winrate wr}))
         y (iterate inc 1)))) ;counting the moves from 1

(defn sgf-report
  [sgf]
  (let [flp (flat-list-properties sgf)
        black (extract-single-value flp "PB")
        white (extract-single-value flp "PW")
        result (extract-single-value flp "RE")
        raw (raw-data flp)
        title (str "B: " black " W: " white " R: " result)]
    (game-report raw title)))
