(ns lgo.analysis.oz
  "Functions for visualization of  the output of a KataGo analysis, either
  through Lizzie or by the KataGo Analysis Engine directly.

  Design decisions:
  Internally all score values are from Black's perspective: positive means Black
  win, negative means White win.
  Move counter tells how many moves were made. Color tells whose turn is it.

  The raw data is a hash-map with keys color, move, mean, meanmean, medianmean,
  means. This is the input of the oz visualization."
  (:require [clojure.string :as string]
            [trptcolin.versioneer.core :as version]
            [lgo.stats :refer [median mean]]))

(defn unroll-scoremeans
  "All score means from raw data. This is just unrolling the means vector
  into separate rows."
  [dat]
  (mapcat
   (fn [d]
     (for [m (:means d)]
       {:color (:color d)
        :move (:move d)
        :mean (if (= "B" (:color d))
                m
                (- m))}))
   dat))

(defn effects
  "The score mean differences caused by the moves."
  [dat]
  (map (fn [[{c1 :color m1 :mean}
             {m2 :mean v2 :move}]]
         (let [eff (if (= c1 "B") ; need to negate for White
                     (- m2 m1)
                     (- (- m2 m1)))]
           {:color c1 :effect eff :move v2}))
       (partition 2 1 dat)))

(defn choices
  [dat]
  (let [ps (partition 2 1 dat)]
    (map (fn [[{c1 :color  m1 :mean mm :meanmean md :medianmean v1 :move}
               {c2 :color m2 :mean v2 :move}]]
           (if (= "B" c1)
             {:color c1 :choice m2 :move v1 :average mm :median md :AI m1}
             {:color c1 :choice (- m2) :move v1 :average (- mm) :median (- md) :AI (- m1)}
             ))
         ps)))

(defn deviations
  [effs]
  (let [avg (mean (map :effect effs))]
    (map (fn [{e :effect :as d}]
           (into d [[:deviation (- e avg)]]))
         effs)))


(defn data-transform
  "Prepares database for plotting in the same diagram. Fixed keys are copied,
  then the variable ones are added as 'name' and its value under kw."
  [db fixedkeys varkeys kw]
  (mapcat (fn [row]
            (let [fixedvals (into {} (map (fn [k] [k (k row)]) fixedkeys))]
              (map (fn [k] (into fixedvals
                                 [[:name (name k)] [kw (k row)]]))
                   varkeys)))
          db))

;; Oz visualization functions producing vega-lite specifications
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
 (let [N (count e-d)
       cmsm (reductions + (map :effect e-d))
       normalized (map (fn [d v]
                         (into d
                               [[:cumsum
                                 (if (> (:move d) 1)
                                   (/ v (/ (:move d) 2))
                                   v)]]))
                       e-d cmsm)]
   normalized))

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
  [raw title]
  (let [all-sm (unroll-scoremeans raw)
        effs-dat (effects raw)
        dev-dat (deviations effs-dat)
        cs (choices raw)
        tcs (data-transform cs [:color :move]
                            [:choice :median :AI :average] :scoreMean)
        N (count effs-dat)
        w (int (* 5.4 N))]
    [:div
     [:h1 title]
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

;; general plotting functions
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
