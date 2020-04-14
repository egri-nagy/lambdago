(ns lgo.lizzie
  "Functions for working with the output of Lizzie after doing KataGo analysis."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.matrix.stats :refer [mean sd]]
            [lgo.sgf :refer [flat-list-properties
                             extract-properties
                             extract-property
                             extract-single-value]]))

(defn extract-from-LZ
  "Simply extracts from LZ string s the values after the name.
  Just he one after, so it is not good for extracting PV moves."
  [s name]
  (map second
       (filter #(= name (first %))
               (partition 2 1
                          (clojure.string/split s #" ")))))

(defn raw-data
  [flp]
  (let [x (map (fn [[id val]]
                 (if (#{"B" "W"} id)
                   id
                   (mapv read-string
                        (extract-from-LZ val "scoreMean"))))
               (extract-properties flp #{"B" "W" "LZ"}))
        y (partition 2 x)]
    (map (fn [[player means] move]
           {:move move :color player :mean (first means) :meanmean (mean means) :means means})
         y (range 1 1000))))

(defn extract-score-means
  [flp]
  (let [x (map (fn [[id val]]
                 (if (#{"B" "W"} id)
                   id
                   (read-string
                    (first
                     (extract-from-LZ val "scoreMean")))))
               (extract-properties flp #{"B" "W" "LZ"}))
        y (partition 2 x)]
    (map (fn [[player mean] move]
           [player mean move])
         y (range 1 1000))))

(defn extract-all-score-means
  [flp]
  (let [LZs (extract-property flp "LZ")
        raw (map
             (fn [[id val]]
               (map read-string
                    (extract-from-LZ val "scoreMean")))
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


;; Oz visualization functions producing vega-lite specifications
(defn oz-effects
  [e-d w]
  {:data {:values e-d}

   :vconcat[{:encoding {:x {:field "move" :type "quantitative"}
                        :y {:field "effect" :type "quantitative"} :color {:field "color" :type "nominal"}}
             :mark "bar" :width w}
            {:encoding {:x {:field "move" :type "quantitative"}
                        :y {:field "cumsum" :type "quantitative"} :color {:field "color" :type "nominal"}}
             :mark "bar" :width w}]})

(defn oz-normalized-effects
  [e-d w]
  (let [N (count e-d)
        normalized (map
                    (fn [m]
                      (update m :cumsum (fn [e] (/ e N))))
                    e-d)]
    {:data {:values normalized}
     :encoding {:x {:field "move" :type "quantitative"}
                :y {:field "cumsum" :type "quantitative"} :color {:field "color" :type "nominal"}}
     :mark "bar" :width w}))

(defn oz-average-effect
  [e-d]
  {:data {:values e-d}
   :encoding {:x {:field "color" :type "nominal"}
              :y {:aggregate "mean" :field "effect" :type "quantitative"}}
   :mark "bar"})

(defn oz-sd-effect
  [e-d]
  {:data {:values e-d}
   :encoding {:x {:field "color" :type "nominal"}
              :y {:aggregate "stdev" :field "effect" :type "quantitative"}}
   :mark "bar"})

(defn oz-min-max
  [e-d]
  {:data {:values e-d}
   :layer [{:encoding {:x {:field "color" :type "nominal"}
                       :y {:aggregate "max" :field "effect" :type "quantitative"}
                       }
            :mark "bar"}
           {:encoding {:x {:field "color" :type "nominal"}
                       :y {:aggregate "min" :field "effect" :type "quantitative"}
                       }
            :mark "bar"}]})


(defn oz-scoremeans
  [flp w]
  {:data {:values (extract-all-score-means flp)}
   :width w
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
        effs-dat (effects-data flp)
        N (count effs-dat)
        w (int (* 5.4 N))]
    [:div
     [:h1 (str "B: " black " W: " white) " R: " result]
     [:p "All scoreMean values for indicating volatility."]
     [:vega-lite (oz-scoremeans flp w)]
     [:vega-lite (oz-effects effs-dat w)]
     [:p "Normalized by the number of moves."]
     [:vega-lite (oz-normalized-effects effs-dat w)]
     [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite (oz-average-effect effs-dat)]
      [:vega-lite (oz-sd-effect effs-dat)]
      [:vega-lite (oz-min-max effs-dat)]]]))
