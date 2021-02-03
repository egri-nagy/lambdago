(ns lgo.analysis.oz
  "Functions for visualization of  the output of a KataGo analysis, either
  through Lizzie or by the KataGo Analysis Engine directly."
  (:require [trptcolin.versioneer.core :as version]
            [lgo.stats :refer [mean cmas]]
            [lgo.analysis.processing :refer [unroll-scoremeans
                                             effects
                                             effects-with-cost-of-passing
                                             choices
                                             deviations
                                             cost-of-passing
                                             efficiency]]))

(declare scatterplot)

(defn data-transform
  "Prepares a database for plotting in the same diagram. Fixed keys are copied,
  then the variable ones are added as 'name' and its value under kw."
  [db fixedkeys varkeys kw]
  (mapcat (fn [row]
            (let [fixedvals (into {} (map (fn [k] [k (k row)]) fixedkeys))]
              (map (fn [k] (into fixedvals
                                 [[:name (name k)] [kw (k row)]]))
                   varkeys)))
          db))

;; Oz visualization functions producing vega-lite specifications
(defn oz-cops
  [cops w t]
  {:data {:values cops}
   :layer[{:encoding {:x {:field "move" :type "ordinal"}
                      :y {:field "cop" :type "quantitative"} :color {:field "color" :type "nominal"}}
           :mark "bar" :width w :title t}
          {:encoding {
                      :y {:field "cop" :type "quantitative" :aggregate "mean"} :color {:field "color" :type "nominal"}}
           :mark "rule"}
          ]})



(defn oz-effects
  [e-d w t]
  {:data {:values e-d}
   :layer[{:encoding {:x {:field "move" :type "ordinal"}
                        :y {:field "effect" :type "quantitative"} :color {:field "color" :type "nominal"}}
             :mark "bar" :width w :title t}
          {:encoding {
                        :y {:field "effect" :type "quantitative" :aggregate "mean"} :color {:field "color" :type "nominal"}}
             :mark "rule"}
            ]})

(defn oz-deviations
  [e-d w t]
  {:data {:values e-d}
   :vconcat[{:encoding {:x {:field "move" :type "ordinal"}
                        :y {:field "deviation" :type "quantitative"}}
             :mark "bar" :width w :title t}]})

(defn normalize-effects
 "assuming that it is from one player"
 [e-d]
  (let [avgs (cmas (map :effect e-d))]
    (map (fn [d v]
           (conj d [:cumsum v]))
         e-d avgs)))

(defn oz-normalized-effects
  [e-d w t]
  {:data {:values (normalize-effects e-d)}
   :encoding {:x {:field "move" :type "quantitative"}
              :y {:field "cumsum" :type "quantitative"}
              :color {:field "color" :type "nominal"}}
   :mark "bar" :width w :title t})

(defn oz-normalized-effects2
  [data w t]
  {:data {:values data}
   :encoding {:x {:field "move" :type "quantitative"}
              :y {:field "cumsum" :type "quantitative"}
              :color {:field "color" :type "nominal"}}
   :mark "bar" :width w :title t})


(defn oz-effects-summary
  [e-d]
  {:data {:values e-d}
   :title "Summary of effects"
   :encoding {:x {:field "color" :type "nominal"}
              :y {:field "effect" :type "quantitative"}}
   :mark {:type "boxplot" :extent "min-max"}})

(defn oz-all-scoremeans
  [d w t]
  {:data {:values d}
   :width w
   :title t
   :encoding {:x {:field "move" :type "ordinal"}
              :y {:field "mean" :type "quantitative"}}
   :mark {:type "boxplot" :extent "min-max" :size 5}})


(defn oz-choices
  [c w t]
  {:data {:values c}
   :encoding {:x {:field "move" :type "ordinal"}
              :y {:field "scoreMean" :type "quantitative"}
              :color {:field "name" :type "nominal"}}
   :mark {:type "line" :size 1}  :width w :title t })


(defn game-report
  [RAW title]
  (let [raw (:game RAW) ;; the game entries
        passed (:passed RAW) ;; entries for the artificially passed game
        copd (cost-of-passing RAW)
        effcs (efficiency RAW copd)
        all-sm (unroll-scoremeans raw)
        effs-dat (effects raw)
        dev-dat (deviations effs-dat)
        cs (choices raw)
        tcs (data-transform cs [:color :move]
                            [:choice :median :AI :average] :scoreMean)
        N (count effs-dat)
        w (int (* 5.4 N))]
    [:div
     [:h1 title]
     [:vega-lite (oz-cops copd w "Cost of passing")]
     [:vega-lite (oz-cops effcs w "Efficiency")]
     [:vega-lite {:data {:values raw}
                  :vconcat[{:encoding {:x {:field "move" :type "ordinal"}
                                       :y {:field "winrate" :type "quantitative"}}
                            :mark "line" :width w :title "winrate"}]}]
     [:p "Move numbers for score means indicate how many moves made before."]
     [:vega-lite (oz-choices
                  (filter #(= "B" (:color %)) tcs)
                  w
                  "Black's scoremean values")]
     [:vega-lite (oz-choices
                  (filter #(= "W" (:color %)) tcs)
                  w
                  "White's scoremean values")]
     [:vega-lite (oz-all-scoremeans
                  (filter #(= "B" (:color %)) all-sm)
                  w
                  "Black's all scoreMeans for variations")]
     [:vega-lite (oz-all-scoremeans
                  (filter #(= "W" (:color %)) all-sm)
                  w
                  "White's all scoreMeans for variations")]
     [:vega-lite (oz-effects effs-dat w "Effects of moves")]
     [:vega-lite (oz-effects (effects-with-cost-of-passing  effs-dat copd)
                             w
                             "Effects of moves divided by cost of passing")]
     [:vega-lite (oz-effects (filter #(= "W" (:color %)) effs-dat) w "Effects of White's moves")]
     [:vega-lite (oz-effects (filter #(= "B" (:color %)) effs-dat) w "Effects of Black's moves")]
     [:vega-lite (oz-deviations (filter #(= "W" (:color %)) dev-dat) w "Deviations (distances from the mean) of White's moves")]
     [:vega-lite (oz-deviations (filter #(= "B" (:color %)) dev-dat) w "Deviations of Black's moves")]
     [:vega-lite
      (oz-normalized-effects (filter #(= "W" (:color %)) effs-dat)  w
                             "White's cumulative moving average of effects")]
     [:vega-lite
      (oz-normalized-effects (filter #(= "B" (:color %)) effs-dat)  w
                             "Black's cumulative moving average of effects")]
     [:vega-lite
      (oz-normalized-effects2 (concat (normalize-effects (filter #(= "W" (:color %)) effs-dat))
                                      (normalize-effects (filter #(= "B" (:color %)) effs-dat)))
                              w
                             "Cumulative moving average of effects")]

     [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite (oz-effects-summary effs-dat)]]
     [:p "report generated by LambdaGo v"
      (version/get-version "lambdago" "lambdago")]]))

;; general plotting functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn scatterplot
  [xs xlabel ys ylabel]
  {:data {:values (map (fn [x y] {(keyword xlabel) x (keyword ylabel) y}) xs ys)}
   :encoding {:x {:field xlabel :type "quantitative"}
              :y {:field ylabel :type "quantitative"}}
   :mark "point" :width 1000 :height 1000})

(defn boxplots
  [data category measurement]
  {:data {:values data},
   :mark {:type "boxplot",
          :extent 1.5},
   :encoding {
              :y {:field category, :type "nominal"},
              :x {
                  :field measurement,
                  :type "quantitative",
                  ;:scale: {:zero  false}
                  }}})
