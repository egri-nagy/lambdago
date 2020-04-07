(ns lgo.sgf
  "Functions for working with the Smart Game Format.
  Extracting positions and move sequences.
  Exporting to the (unreleased) goban LaTeX package."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.matrix.stats :refer [mean sd]]))

;; a crude parser for SGF files for extracting property values
(def SGFparser (insta/parser
                (str "GameRecord = GameTree { GameTree };\n"
                     "GameTree   = <\"(\"> Sequence { GameTree } <\")\">;\n"
                     "Sequence   = Node { Node };\n"
                     "Node = <\";\"> { Property };\n"
                     "Property   = Identifier Value { Value };\n"
                     "Identifier  = #'[A-Z]+';\n"
                     "Value  = <\"[\"> #\"[^\\]]*\" <\"]\">")))

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

(defn extract-properties [fpt p]
  (filter #(and (vector? %) (p (first %)))
          (tree-seq vector? identity fpt)))

(with-precision 10 :rounding FLOOR (/ 1 3M))



(defn extract-game-moves
  [sgf]
  (extract-properties (flattened-parse-tree sgf)
                      #(or (= % "B") (= % "W"))))

(defn round3 [f]
  (float (/ (int (Math/round (* 1000 f))) 1000)))

;; this will get the move and first scoreMean out of lizzie analysis
(defn extract-LZ
  [sgf]
  (extract-properties (flattened-parse-tree sgf)
                      #(or (= % "B") (= % "W") (= % "LZ"))))

(defn extract-score-means
  [sgf]
  (let [x (map #(if (= 1 (count (first %)))
                  (first %)
                  (read-string (nth (string/split (second %) #" ") 9)))
                (lgo.sgf/extract-LZ sgf))
        y (partition 2 x)]
    (map (fn [[player mean] move]
           [player mean move])
         y (range 1 1000))))

(defn effects
  [sgf]
  (let [ms (map (fn [[c m v]] (if (= c "B")  [c (* -1 m) v] [c m v]) )
                (extract-score-means sgf))
        ps (partition 2 1 ms)]
    (map (fn [[[c1 m1 v1] [c2 m2 v2]]] (if (= c2 "W") [c2 (round3 (-(- m2 m1))) v2]
                                     [c2 (round3 (- m2 m1)) v2]))
         ps)))

(defn analysis
  [sgf]
  (let [effs (effects sgf)
        Beffs (map second (filter (comp (partial = "B") first) effs))
        Weffs (map second (filter (comp (partial = "W") first) effs))]
    (println "Black avg sd total " ((juxt mean sd) Beffs) (reduce + Beffs))
    (println "White avg sd total " ((juxt mean sd) Weffs) (reduce + Weffs))))

(defn effects-data-for-one-color
  [sgf color]
  (let [dat (filter (comp (partial = color) first)
                     (effects sgf))
        effs (map second dat)
        moves (map #(nth % 2) dat)
        cumsum (reductions + effs)]
    (map (fn [m e s] {:move m :color color :effect e :cumsum s})
         moves effs cumsum)))

(defn effects-data
  [sgf]
  (concat (effects-data-for-one-color sgf "W")
          (effects-data-for-one-color sgf "B")))

(defn oz-effects
  [sgf]
  {:data {:values (effects-data sgf)}

   :vconcat[{:encoding {:x {:field "move" :type "quantitative"}
                      :y {:field "cumsum" :type "quantitative"} :color {:field "color" :type "nominal"}}
             :mark "bar" :width 1200}
            {:encoding {:x {:field "move" :type "quantitative"}
                        :y {:field "effect" :type "quantitative"} :color {:field "color" :type "nominal"}}
             :mark "bar" :width 1200}
            {:encoding {:x {:field "color" :type "nominal"}
                        :y {:aggregate "mean" :field "effect" :type "quantitative"}
                        }
             :mark "bar"}
            {:encoding {:x {:field "color" :type "nominal"}
                        :y {:aggregate "stdev" :field "effect" :type "quantitative"}
                        }
             :mark "bar"}

            {:layer [{:encoding {:x {:field "color" :type "nominal"}
                        :y {:aggregate "max" :field "effect" :type "quantitative"}
                        }
             :mark "bar"}
            {:encoding {:x {:field "color" :type "nominal"}
                        :y {:aggregate "min" :field "effect" :type "quantitative"}
                        }
             :mark "bar"}]}]})

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
