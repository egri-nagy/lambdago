(ns lgo.lizzie
  "Functions for working with the output of Lizzie after doing KataGo analysis."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.matrix.stats :refer [mean sd]]
            [lgo.sgf :refer [flat-list-properties
                             extract-properties
                             extract-property
                             extract-single-value]]))

(defn round3 [f]
  (float (/ (int (Math/round (* 1000 f))) 1000)))

;; this will get the move and first scoreMean out of lizzie analysis
(defn extract-LZ
  [flp]
  (extract-properties flp #{"B" "W" "LZ"}))

(defn extract-score-means
  [flp]
  (let [x (map
           (fn [[id val]]
             (if (#{"B" "W"} id)
               id
               (read-string
                (first
                 (map second
                      (filter #(= "scoreMean" (first %))
                              (partition 2 1
                                         (clojure.string/split val #" "))))))))
           (extract-LZ flp))
        y (partition 2 x)]
    (map (fn [[player mean] move]
           [player mean move])
         y (range 1 1000))))

(defn extract-all-score-means
  [flp]
  (let [LZs (extract-property flp "LZ")
        raw (map
             (fn [[id val]]
               (map (comp read-string second)
                    (filter #(= "scoreMean" (first %))
                            (partition 2 1
                                       (clojure.string/split val #" ")))))
             LZs)
        corrected (mapv (fn [vals f] (map f vals))
                        raw
                        (cycle [identity (partial * -1)]))]
    (for [m (range 0 (count corrected))
          v (corrected m)]
      {:move (inc m) :scoremean v})))

(defn effects
  [flp]
  (let [ms (map (fn [[c m v]] (if (= c "B")  [c (* -1 m) v] [c m v]) )
                (extract-score-means flp))
        ps (partition 2 1 ms)]
    (map (fn [[[c1 m1 v1] [c2 m2 v2]]] (if (= c2 "W") [c2 (-(- m2 m1)) v2]
                                           [c2 (- m2 m1) v2]))
         ps)))

(defn analysis
  [sgf]
  (let [effs (effects sgf)
        Beffs (map second (filter (comp (partial = "B") first) effs))
        Weffs (map second (filter (comp (partial = "W") first) effs))]
    (println "Black avg sd total " ((juxt mean sd) Beffs) (reduce + Beffs))
    (println "White avg sd total " ((juxt mean sd) Weffs) (reduce + Weffs))))

(defn effects-data-for-one-color
  [flp color]
  (let [dat (filter (comp (partial = color) first)
                    (effects flp))
        effs (map second dat)
        moves (map #(nth % 2) dat)
        cumsum (reductions + effs)]
    (map (fn [m e s] {:move m :color color :effect e :cumsum s})
         moves effs cumsum)))

(defn effects-data
  [flp]
  (concat (effects-data-for-one-color flp "W")
          (effects-data-for-one-color flp "B")))

(defn oz-effects
  [e-d]
  {:data {:values e-d}

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

(defn oz-scoremeans
  [flp]
  {:data {:values (extract-all-score-means flp)}
   :width 1200
   :layer [{:encoding {:x {:field "move" :type "quantitative"}
                        :y {:field "scoremean" :type "quantitative"}}
             :mark "point"}
           {:encoding {:x {:field "move" :type "quantitative"}
                       :y {:field "scoremean" :type "quantitative"
                           :aggregate "mean"}}
            :mark {:type "line" :color "orange"} }]})

(defn sgf-report
  [sgf]
  (let [flp (flat-list-properties sgf)
        black (extract-single-value flp "PB")
        white (extract-single-value flp "PW")
        result (extract-single-value flp "RE")
        effs-dat (effects-data flp)]
    [:div
     [:h1 (str "B: " black " W: " white) " R: " result]
     ;[:p "These are the extracted scoreMeans, not separated by color, hence the branches."]
     [:vega-lite (oz-scoremeans flp)]
     [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite (oz-effects effs-dat)]]]))
