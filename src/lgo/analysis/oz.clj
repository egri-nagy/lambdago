(ns lgo.analysis.oz
  "Functions for visualization of  the output of a KataGo analysis, either
  through Lizzie or by the KataGo Analysis Engine directly."
  (:require [trptcolin.versioneer.core :as version]
            [meander.epsilon :refer [match]]
            [lgo.stats :refer [cmas]]
            [lgo.analysis.processing :refer [unroll-scoremeans
                                             effects
                                             choices
                                             deviations
                                             cost-of-passing
                                             effect-vs-cop]]))

(declare scatterplot)

(def sign-swap (partial * -1))

;;further processing functions - TODO: check whether they belong to processing
(defn data-transform
  "Declarative version of data-transform2."
  [db]
  (mapcat
   (fn [m]
     (match m
            ; this is the input, the properties have their own keys
            {:color ?color :move ?move
             :choice ?choice :average ?average :median ?median :AI ?AI}
            ; the output: each property is separated into a map
            [{:color ?color :move ?move :name "choice" :scoreMean ?choice}
             {:color ?color :move ?move :name "average" :scoreMean ?average}
             {:color ?color :move ?move :name "median" :scoreMean ?median}
             {:color ?color :move ?move :name "AI" :scoreMean ?AI}]))
   db))

(defn data-transform2
  "Prepares a database of different mean values for plotting in the same diagram.
  Fixed keys are copied, then the variable ones are added as 'name' and its
  value under kw."
  [db fixedkeys varkeys kw]
  (mapcat
   (fn [row]
     (let [fixedvals (into {} (map (fn [k] [k (k row)]) fixedkeys))]
       (map (fn [k] (into fixedvals [[:name (name k)] [kw (k row)]]))
            varkeys)))
   db))

(defn normalize-effects
  "assuming that it is from one player"
  [e-d]
  (let [avgs (cmas (map :effect e-d))]
    (map (fn [d v]
           (conj d [:cumsum v]))
         e-d avgs)))

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
  "Generic bar graph for displaying a quantity for each move."
  [dat field width title]
  {:data {:values dat}
   :encoding {:x {:field "move" :type "ordinal" :axis {:values (range 10 1001 10)}}
              :y {:field field :type "quantitative"}
              :fill color-coded-fill
              :stroke black-strokes
              :tooltip [{:field "move" :type "ordinal"}
                        {:field field :type "quantitative"}]}
   :mark "bar"
   :width width
   :title title})

(defn oz-boxplot-summary
  "Generic boxplot for a quantity separately for both players."
  [dat field title]
  {:data {:values dat}
   :title title
   :encoding {:x {:field "color" :type "nominal"}
              :y {:field field :type "quantitative"}
              :fill color-coded-fill
              :stroke gray-strokes}
   :mark {:type "boxplot" :extent "min-max"}})

;;the functions below remain to be specifi - maybe that is ok
(defn oz-all-scoremeans
  [d w t]
  {:data {:values d}
   :width w
   :title t
   :encoding {:x {:field "move" :type "ordinal"}
              :y {:field "mean" :type "quantitative"}
              :fill color-coded-fill
              :stroke gray-strokes}
   :mark {:type "boxplot" :extent "min-max" :size 5}})

(defn oz-choices
  [c w t]
  {:data {:values c}
   :encoding {:x {:field "move" :type "quantitative"}
              :y {:field "scoreMean" :type "quantitative"}
              :color {:field "name" :type "nominal"}}
   :mark {:type "line" :point true :tooltip {:content "data"}}  :width w :title t })

(defn game-report
  [RAW title]
  (let [raw (:game RAW) ;; the game entries
        ;passed (:passed RAW) ;; entries for the artificially passed game
        copd (reduce
              (fn [d m]
                (if (= "black" (:color m))
                  (conj d m)
                  (conj d (update m :cop sign-swap))))
              []
              (cost-of-passing RAW))
        all-sm (unroll-scoremeans raw)
        ;;effects
        raw-effs (effects raw)
        white? #(= "white" (:color %))
        black? #(= "black" (:color %))
        black-effs-dat (filter black? raw-effs)
        white-effs-dat (map
                        (fn [d] (update d :effect sign-swap))
                        (filter white? raw-effs))
        effs-dat (concat black-effs-dat white-effs-dat)
        ;;choices
        cs (choices raw)
        tcs (data-transform2 cs [:color :move] [:choice :median :AI :average] :scoreMean)
        N (count effs-dat)
        w (int (* 5.4 N))
        h (int (* 2 N))]
    [:div
     [:p "LambdaGo v" (version/get-version "lambdago" "lambdago")]
     [:h1 title]
     [:p "Move numbers for score means indicate how many moves made before."]
     (when-not (empty? copd)
       (let [d (map
                (fn [m] (if (= (:color m) "white")
                          (update m :effect sign-swap)
                          m))
                (effect-vs-cop raw-effs copd))]
         [:vega-lite
          ;(scatterplot (map :effect d) "effect" (map :cop d) "cop")
          (let [l (map :cop d)
                a (apply min l)
                b (apply max l)] 
            {:data {:values d}
             :encoding {:x {:field "cop" :type "quantitative" :scale {:domain [a b]} }
                        :y {:field "effect" :type "quantitative"}
                        :color {:field "color" :type "nominal"}
                        :fill color-coded-fill :stroke gray-strokes
                        }
             :mark ;"point"
             {:type "point" :point true :tooltip {:content "data"}}
             :width w :height h :title "Cost of passing versus effect (more info in tooltips)"
             })
          ]))
     (when-not (empty? copd)
       [:vega-lite
                                        ;(scatterplot (map :effect d) "effect" (map :cop d) "cop")
        (let [l (map :cop copd)
              a (apply min l)
              b (apply max l)] 
          {:data {:values copd}
           :encoding {:x {:field "cop" :type "quantitative" :scale {:domain [a b]} }
                      :y {:field "efficiency" :type "quantitative"}
                      :color {:field "color" :type "nominal"}
                      :fill color-coded-fill :stroke gray-strokes
                      }
           :mark ;"point"
           {:type "point" :point true :tooltip {:content "data"}}
           :width w :height h :title "Cost of passing versus efficiency (more info in tooltips)"
           })
        ])
     (when-not (empty? copd)
       [:vega-lite (oz-bars-per-move copd "cop" w "Cost of passing")])
     (when-not (empty? copd)
       [:vega-lite (oz-bars-per-move
                    (map #(update % :move inc) copd) ;quick hack as move is off by one
                    "efficiency" w "Efficiency - how much percent of the score in cost of passing realized? (tops at 151%)")])
     [:vega-lite {:data {:values raw}
                  :vconcat[{:encoding {:x {:field "move" :type "quantitative"}
                                       :y {:field "winrate" :type "quantitative"}}
                            :mark {:type "line" :point true :tooltip {:content "data"}}  :width w :title "winrate"}]}]
     [:vega-lite (oz-choices (filter black? tcs) w "Black's scoremean values")]
     [:vega-lite (oz-choices (filter white? tcs) w "White's scoremean values")]
     [:vega-lite (oz-all-scoremeans (filter black? all-sm) w
                                    "Black's all scoreMeans for variations")]
     [:vega-lite (oz-all-scoremeans (filter white? all-sm) w
                                    "White's all scoreMeans for variations")]
     [:h2 "Effects"]
     [:vega-lite (oz-bars-per-move effs-dat "effect" w "Effects of moves")]
     [:vega-lite (oz-bars-per-move white-effs-dat "effect" w
                                   "Effects of White's moves")]
     [:vega-lite (oz-bars-per-move black-effs-dat "effect" w
                                   "Effects of Black's moves")]

     [:h2 "Cumulative moving averages of effects"]
     [:vega-lite
      (oz-bars-per-move (normalize-effects (filter white? effs-dat)) "cumsum" w
                        "Cumulative moving average of effects (white)")]
     [:vega-lite
      (oz-bars-per-move (normalize-effects  (filter black? effs-dat)) "cumsum" w
                        "Cumulative moving average of effects (black)")]
     [:vega-lite
      (oz-bars-per-move (concat (normalize-effects (filter white? effs-dat))
                                (normalize-effects (filter black? effs-dat)))
                        "cumsum" w
                        "Cumulative moving average of effects (composite)")]
     [:vega-lite (oz-bars-per-move (deviations white-effs-dat) "deviation" w
                                   "Deviations from meean effect (white)")]
     [:vega-lite (oz-bars-per-move (deviations black-effs-dat) "deviation" w
                                   "Deviations from mean effect (black)")]
     [:h2 "Summaries"]
     [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite (oz-boxplot-summary effs-dat "effect" "Summary of effects")]
      (when-not (empty? copd) [:vega-lite (oz-boxplot-summary copd "cop" "Summary of cost of passings")])]
     ]))

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
