(ns lgo.analysis.vl
  "Functions for generating Vega-Lite code from game information.
   Migration method: implement the functions here from lgo.analysis.oz
   and then delete that namespace.")

;;Some hardcoded values
(def diagram-height 120)
(def diagram-font-size 16)

;; Vega-Lite fragments for drawing styles
(def color-coded-fill {:field "color"
                       :type "nominal"
                       :scale {:range {:field "color"}}})
(def black-strokes  {:field "color" :type "nominal"
                     :scale {:range ["black" "black"]}})
(def gray-strokes  {:field "color" :type "nominal"
                     :scale {:range ["gray" "gray"]}})
(def axis-font {:labelFontSize diagram-font-size
                :titleFontSize diagram-font-size
                :titleFontWeight "normal"})

;; Vega-Lite specification for bar chart with moves as the x-axis
(defn vl-bars-per-move
  "Generic bar graph for displaying a quantity for each move."
  [dat field width title]
  {:data {:values dat}
   :encoding {:x {:field "move"
                  :type "ordinal"
                  :axis (conj axis-font [:values (range 10 1001 10)])}
              :y {:field field :type "quantitative"
                  :axis axis-font}
              :fill color-coded-fill
              :stroke black-strokes
              :tooltip [{:field "move" :type "ordinal"}
                        {:field field :type "quantitative"}]}
   :mark "bar"
   :width width
   :height diagram-height
   :title {:text title
           :fontSize diagram-font-size
           :fontWeight "normal"}})

(defn vl-boxplot-summary
  "Generic boxplot for a quantity separately for both players."
  [dat field title]
  {:data {:values dat}
   :title {:text title :fontSize 16 :fontWeight "normal"}
   :encoding {:x {:field "color" :type "nominal"}
              :y {:field field :type "quantitative"}
              :fill color-coded-fill
              :stroke gray-strokes}
   :mark {:type "boxplot" :extent "min-max"}})

;;the functions below remain to be specific - maybe that is ok
(defn vl-all-scoremeans
  [d w t]
  {:data {:values d}
   :width w
   :title t
   :encoding {:x {:field "move" :type "ordinal"}
              :y {:field "mean" :type "quantitative"}
              :fill color-coded-fill
              :stroke gray-strokes}
   :mark {:type "boxplot" :extent "min-max" :size 5}})

(defn vl-choices
  [c w t]
  {:data {:values c}
   :encoding {:x {:field "move" :type "quantitative"}
              :y {:field "scoreMean" :type "quantitative"}
              :color {:field "name" :type "nominal"}}
   :mark {:type "line" :point true :tooltip {:content "data"}}  :width w :title t })