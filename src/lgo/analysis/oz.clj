(ns lgo.analysis.oz
  "Functions for visualization of  the output of a KataGo analysis, either
  through Lizzie or by the KataGo Analysis Engine directly."
  (:require [trptcolin.versioneer.core :as version]
            [lgo.stats :refer [cmas]]
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
  (mapcat
   (fn [row]
     (let [fixedvals (into {} (map (fn [k] [k (k row)]) fixedkeys))]
       (map (fn [k] (into fixedvals [[:name (name k)] [kw (k row)]]))
            varkeys)))
   db))

;; Oz fragments for drawing styles
(def color-coded-fill {:field "color"
                       :type "nominal"
                       :scale {:range {:field "color"}}})
(def black-strokes  {:field "color" :type "nominal"
                     :scale {:range ["black" "black"]}})
(def gray-strokes  {:field "color" :type "nominal"
                     :scale {:range ["gray" "gray"]}})



;; Oz visualization functions producing vega-lite specifications
(defn oz-bars-per-move
  [dat field width title]
  {:data {:values dat}
   :encoding {:x {:field "move" :type "quantitative"}
              :y {:field field :type "quantitative"}
              :fill color-coded-fill
              :stroke black-strokes
              :tooltip [{:field "move" :type "ordinal"}
                        {:field field :type "quantitative"}]}
   :mark "bar"
   :width width
   :title title})

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
  [data w t]
  {:data {:values data}
   :encoding {:x {:field "move" :type "quantitative"}
              :y {:field "cumsum" :type "quantitative"}
              :color {:field "color" :type "nominal"}}
   :mark "bar" :width w :title t})


(defn oz-boxplot-summary
  [dat field title]
  {:data {:values dat}
   :title title
   :encoding {:x {:field "color" :type "nominal"}
              :y {:field field :type "quantitative"}
              :fill color-coded-fill
              :stroke gray-strokes}
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
        ;passed (:passed RAW) ;; entries for the artificially passed game
        copd (cost-of-passing RAW)
        effcs (efficiency RAW copd)
        all-sm (unroll-scoremeans raw)
        effs-dat (effects raw)
        white? #(= "white" (:color %))
        black? #(= "black" (:color %))
        black-effs-dat (filter black? effs-dat)
        white-effs-dat (filter white? effs-dat)
        cs (choices raw)
        tcs (data-transform cs [:color :move]
                            [:choice :median :AI :average] :scoreMean)
        N (count effs-dat)
        w (int (* 5.4 N))
       ]
    [:div
     [:h1 title]
     [:p "Move numbers for score means indicate how many moves made before."]
     [:vega-lite (oz-bars-per-move copd "cop" w "Cost of passing")]
     [:vega-lite (oz-bars-per-move effcs "cop"
                          w
                          "Efficiency - how much percent of the score in cost of passing realized?")]
     [:vega-lite {:data {:values raw}
                  :vconcat[{:encoding {:x {:field "move" :type "ordinal"}
                                       :y {:field "winrate" :type "quantitative"}}
                            :mark "line" :width w :title "winrate"}]}]
     [:vega-lite (oz-choices
                  (filter black? tcs)
                  w
                  "Black's scoremean values")]
     [:vega-lite (oz-choices
                  (filter white? tcs)
                  w
                  "White's scoremean values")]
     [:vega-lite (oz-all-scoremeans
                  (filter black? all-sm)
                  w
                  "Black's all scoreMeans for variations")]
     [:vega-lite (oz-all-scoremeans
                  (filter white? all-sm)
                  w
                  "White's all scoreMeans for variations")]
     [:vega-lite (oz-bars-per-move effs-dat "effect" w "Effects of moves")]
     [:vega-lite (oz-bars-per-move
                  (effects-with-cost-of-passing  effs-dat copd)
                  "effect"
                  w
                  "Effects of moves divided by cost of passing")]
     [:vega-lite (oz-bars-per-move
                  white-effs-dat
                  "effect"
                  w
                  "Effects of White's moves")]
     [:vega-lite (oz-bars-per-move
                  black-effs-dat
                  "effect"
                  w
                  "Effects of Black's moves")]
     [:vega-lite (oz-deviations (deviations white-effs-dat) w "Deviations (distances from the mean) of White's moves")]
     [:vega-lite (oz-deviations (deviations black-effs-dat) w "Deviations of Black's moves")]
     [:vega-lite
      (oz-normalized-effects (normalize-effects (filter white? effs-dat))  w
                             "White's cumulative moving average of effects")]
     [:vega-lite
      (oz-normalized-effects (normalize-effects  (filter black? effs-dat))  w
                             "Black's cumulative moving average of effects")]
     [:vega-lite
      (oz-normalized-effects (concat (normalize-effects (filter white? effs-dat))
                                      (normalize-effects (filter black? effs-dat)))
                              w
                             "Cumulative moving average of effects (composite)")]

     [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite (oz-boxplot-summary effs-dat "effect" "Summary of effects")]
      [:vega-lite (oz-boxplot-summary copd "cop" "Summary of cost of passings")]]
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
