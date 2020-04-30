(ns lgo.lizzie
  "Functions for working with the output of Lizzie after doing KataGo analysis."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.matrix.stats :refer [mean]]
            [lgo.sgf :refer [flat-list-properties
                             extract-properties
                             extract-single-value]]))

(defn median
  [nums]
  (let [ordered (vec (sort nums))
        n (count nums)
        h (dec (int (/ n 2)))
        indices (if (odd? n)
                  [(inc h)]
                  [h (inc h)])]
    (mean (map ordered indices))))

(defn extract-from-LZ
  "Simply extracts from LZ string s the values after the name.
  Just the one after, so it is not good for extracting PV moves."
  [s name]
  (map second
       (filter #(= name (first %))
               (partition 2 1
                          (clojure.string/split s #" ")))))

(defn raw-data
  "Extracts the score means and the color of the previous move."
  [flp]
  (let [x (map (fn [[id val]]
                 (if (#{"B" "W"} id)
                   id
                   (mapv read-string
                        (extract-from-LZ val "scoreMean"))))
               (extract-properties flp #{"B" "W" "LZ"}))
        y (partition 2 x)] ;combining move and score name
    (map (fn [[player means] move]
           {:move move
            :color player
            :mean (first means)
            :meanmean (mean means)
            :medianmean (median means)
            :means means})
         y (range 1 1000))))

(defn extract-all-score-means
  [dat]
  (mapcat
   (fn [d]
     (for [m (:means d)]
       {:color (:color d)
        :move (:move d)
        :mean m}))
   dat))

(defn effects
  [dat]
  (let [ms (map (fn [{c :color :as d}]
                  (if (= c "B")
                    (update d :mean (partial * -1))
                    d))
                dat)
        ps (partition 2 1 ms)]
    (map (fn [[{c1 :color m1 :mean v1 :move}
               {c2 :color m2 :mean v2 :move}]]
           (if (= c2 "W")
             {:color c2 :effect (- (- m2 m1)) :move v2}
             {:color c2 :effect (- m2 m1) :move v2}))
         ps)))

(defn choices
  [dat]
  (let [ms (map (fn [{c :color :as d}]
                  (if (= c "B")
                    (-> d
                         (update :mean (partial * -1))
                         (update :meanmean (partial * -1))
                         (update :medianmean (partial * -1)))
                    d))
                dat)
        ps (partition 2 1 dat)]
    (map (fn [[{ m1 :mean mm :meanmean md :medianmean}
               {c2 :color m2 :mean v2 :move}]]
           {:color c2 :choice (- m2) :move v2 :average mm :median md :best m1})
         ps)))

(defn deviations
  [effs]
  (let [avg (mean (map :effect effs))]
    (map (fn [{e :effect :as d}]
           (into d [[:deviation (- e avg)]]))
         effs)))


(defn data-transform
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

(defn oz-normalized-effects
  [e-d w t]
  (let [N (count e-d)
        cmsm (reductions + (map :effect e-d))
        normalized (map (fn [d v] (into d [[:cumsum (/ v N)]]))
                     e-d cmsm)]
    {:data {:values normalized}
     :encoding {:x {:field "move" :type "quantitative"}
                :y {:field "cumsum" :type "quantitative"} :color {:field "color" :type "nominal"}}
     :mark "bar" :width w :title t}))

(defn oz-aggregate-effect
  [e-d aggr]
  {:data {:values e-d}
   :encoding {:x {:field "color" :type "nominal"}
              :y {:aggregate aggr :field "effect" :type "quantitative"}}
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
  [d w t]
  {:data {:values d}
   :width w
   :title t
   :layer [{:encoding {:x {:field "move" :type "ordinal"}
                       :y {:field "mean" :type "quantitative"}}
            :mark {:type "area",
                   :line { :color "orange"},
                   :color {:x1 1, :y1 1, :x2 1, :y2 0, :gradient "linear",
                           :stops [{:offset 0, :color "white"}
                                   {:offset 1, :color "orange"}]}}}
           {:encoding {:x {:field "move" :type "ordinal"}
                       :y {:field "meanmean" :type "quantitative"}}
            :mark "line"}]})

(defn oz-all-scoremeans
  [d w t]
  {:data {:values d}
   :width w
   :title t
   :encoding {:x {:field "move" :type "ordinal"}
              :y {:field "mean" :type "quantitative"}}
   :mark {:type "point" :shape "circle" :size 3}})


(defn oz-choices
  [c w t]
  {:data {:values c}
   :encoding {:x {:field "move" :type "ordinal"}
              :y {:field "scoreMean" :type "quantitative"}
              :color {:field "name" :type "nominal"}}
   :mark {:type "line" :size 1}  :width w :title t })

(defn sgf-report
  [sgf]
  (let [flp (flat-list-properties sgf)
        black (extract-single-value flp "PB")
        white (extract-single-value flp "PW")
        result (extract-single-value flp "RE")
        raw (raw-data flp)
        all-sm (extract-all-score-means raw)
        effs-dat (effects raw)
        dev-dat (deviations effs-dat)
        cs (choices raw)
        tcs (data-transform cs [:color :move]
                            [:choice :median :best :average] :scoreMean)
        N (count effs-dat)
        w (int (* 5.4 N))]
    [:div
     [:h1 (str "B: " black " W: " white) " R: " result]
     ;[:p "All scoreMean values for indicating volatility."]
     [:vega-lite (oz-choices
                  (filter #(= "B" (:color %)) tcs)
                  w
                  "Black's")]
     [:vega-lite (oz-choices
                  (filter #(= "W" (:color %)) tcs)
                  w
                  "White's")]
     [:vega-lite (oz-scoremeans
                  (filter #(= "W" (:color %)) raw)
                  w
                  "Black's scoreMean and averaged scoreMean for variations")]
     [:vega-lite (oz-scoremeans
                  (filter #(= "B" (:color %)) raw)
                  w
                  "White's scoreMean and averaged scoreMean for variations")]
     [:vega-lite (oz-all-scoremeans
                  (filter #(= "W" (:color %)) all-sm)
                  w
                  "Black's all scoreMeans for variations")]
     [:vega-lite (oz-all-scoremeans
                  (filter #(= "B" (:color %)) all-sm)
                  w
                  "White's all scoreMeans for variations")]
     [:vega-lite (oz-effects effs-dat w "Effects of moves")]
     [:vega-lite (oz-effects (filter #(= "W" (:color %)) effs-dat) w "Effects of White's moves")]
     [:vega-lite (oz-effects (filter #(= "B" (:color %)) effs-dat) w "Effects of Black's moves")]
     [:vega-lite (oz-deviations (filter #(= "W" (:color %)) dev-dat) w "Deviations (distances from the mean) of White's moves")]
     [:vega-lite (oz-deviations (filter #(= "B" (:color %)) dev-dat) w "Deviations of Black's moves")]
     [:vega-lite
      (oz-normalized-effects (filter #(= "W" (:color %)) effs-dat)  w
                             "White's Cumulative sum of effects normalized by number of White moves")]
     [:vega-lite
      (oz-normalized-effects (filter #(= "B" (:color %)) effs-dat)  w
                             "Black's Cumulative sum of effects normalized by number of Black moves")]
     [:div {:style {:display "flex" :flex-direction "row"}}
      [:vega-lite (oz-aggregate-effect effs-dat "mean")]
      [:vega-lite (oz-aggregate-effect effs-dat "stdev")]
      [:vega-lite (oz-min-max effs-dat)]]]))
