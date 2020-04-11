(ns lgo.lizzie
  "Functions for working with the output of Lizzie after doing KataGo analysis."
  (:require [clojure.string :as string]
            [instaparse.core :as insta]
            [clojure.core.matrix.stats :refer [mean sd]]
            [lgo.sgf :refer [flat-list-properties extract-properties]]))

(defn round3 [f]
  (float (/ (int (Math/round (* 1000 f))) 1000)))

;; this will get the move and first scoreMean out of lizzie analysis
(defn extract-LZ
  [sgf]
  (extract-properties (flat-list-properties sgf)
                      #{"B" "W" "LZ"}))

(defn extract-score-means
  [sgf]
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
           (extract-LZ sgf))
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

(defn sgf-report
  [sgf]
  [:div
   [:h1 "Look ye and behold"]
   [:p "A couple of small charts"]
   [:div {:style {:display "flex" :flex-direction "row"}}
    [:vega-lite (oz-effects sgf)]]
   [:p "A wider, more expansive chart"]
   [:h2 "If ever, oh ever a viz there was, the vizard of oz is one because, because, because..."]
   [:p "Because of the wonderful things it does"]])
