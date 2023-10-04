(require '[scicloj.clay.v2.api :as clay])
(require '[scicloj.kindly.v4.kind :as kind])

(clay/start!)

(defn vega-lite-point-plot [data]
  (-> {:data {:values data},
       :mark "point"
       :encoding
       {:size {:field "w" :type "quantitative"}
        :x {:field "x", :type "quantitative"},
        :y {:field "y", :type "quantitative"},
        :fill {:field "z", :type "nominal"}}}
      kind/vega-lite))

(defn random-data [n]
  (->> (repeatedly n #(- (rand) 0.5))
       (reductions +)
       (map-indexed (fn [x y]
                      {:w (rand-int 9)
                       :z (rand-int 9)
                       :x x
                       :y y}))))

(defn random-vega-lite-plot [n]
  (-> n
      random-data
      vega-lite-point-plot))


(clay/handle-value!
 (kind/table {:column-names [:hey :ho]
              :row-vectors [
                            [(kind/vega-lite {:$schema "https://vega.github.io/schema/vega-lite/v5.json"
                                              :data {:values [{:a "A" :b 28}
                                                              {:a "B" :b 55}
                                                              {:a "C" :b 43}
                                                              {:a "D" :b 91}
                                                              {:a "E" :b 81}
                                                              {:a "F" :b 53}
                                                              {:a "G" :b 19}
                                                              {:a "H" :b 87}
                                                              {:a "I" :b 52}]}
                                              :description "A simple bar chart with embedded data."
                                              :encoding {:x {:axis {:labelAngle 0} :field "a" :type "nominal"}
                                                         :y {:field "b" :type "quantitative"}}
                                              :mark "bar"})
                             (random-vega-lite-plot 9)]]}))
