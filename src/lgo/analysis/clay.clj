(ns ^:skip-aot lgo.analysis.clay
  "Functions for generating reports by using clay."
 (:require [lgo.analysis.processing :refer [unroll-scoremeans
                                            effects
                                            choices
                                            deviations
                                            cost-of-passing
                                            effect-vs-cop
                                            normalize-effects
                                            data-transform2]]
           [lgo.analysis.vl :refer [vl-bars-per-move
                                    vl-choices
                                    vl-all-scoremeans
                                    vl-boxplot-summary
                                    color-coded-fill
                                    gray-strokes]]
           [scicloj.clay.v2.api :as clay]
           [scicloj.kindly.v4.kind :as kind]
           [trptcolin.versioneer.core :as version]))

(def sign-swap (partial * -1))

(defn create-game-report
  "Creating the game report as a table (hiccup does not work with vega-lite)
   of the vage-lite visualizations."
  [RAW title]
  (let [raw (:game RAW) ;; the game entries
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
    (kind/table
     {:column-names [title]
      :row-vectors
      [;;header info
       [(str "LambdaGo v" (version/get-version "lambdago" "lambdago" \n)
             "Move numbers for score means indicate how many moves made before."
             (when-not (empty? copd)
               (let [s (apply + (map :cop copd))
                     m (count copd)]
                 (str
                  (format  "Total cop: %.2f" (double s)) " for " m " moves. "
                  (format "Average: %.2f"  (double (/ s m)))))))]
       ;;cop diagrams
       [(kind/vega-lite
         (when-not (empty? copd)
           (let [d (mapv
                    (fn [m] (if (= (:color m) "white")
                              (update m :effect sign-swap)
                              m))
                    (effect-vs-cop raw-effs copd))
                 l (map :cop d)
                 a (apply min l)
                 b (apply max l)]
             {:data {:values d}
              :encoding {:x {:field "cop" :type "quantitative" :scale {:domain [a b]}}
                         :y {:field "effect" :type "quantitative"}
                         :color {:field "color" :type "nominal"}
                         :fill color-coded-fill :stroke gray-strokes}
              :mark ;"point"
              {:type "point" :point true :tooltip {:content "data"}}
              :width w :height h :title "Cost of passing versus effect (more info in tooltips)"})))]
       [(kind/vega-lite
         (when-not (empty? copd)
           (let [l (map :cop copd)
                 a (apply min l)
                 b (apply max l)]
             {:data {:values copd}
              :encoding {:x {:field "cop" :type "quantitative" :scale {:domain [a b]}}
                         :y {:field "efficiency" :type "quantitative"}
                         :color {:field "color" :type "nominal"}
                         :fill color-coded-fill :stroke gray-strokes}
              :mark ;"point"
              {:type "point" :point true :tooltip {:content "data"}}
              :width w :height h :title "Cost of passing versus efficiency (more info in tooltips)"})))]
       [(kind/vega-lite
         (when-not (empty? copd) (vl-bars-per-move copd "cop" w "Cost of passing")))]
       [(kind/vega-lite
          (when-not (empty? copd)
            (vl-bars-per-move
             (mapv #(update % :move inc) copd) ;quick hack as move is off by one
             "efficiency" w "Efficiency - how much percent of the score in cost of passing realized? (tops at 151%)")))]
       [(kind/vega-lite
         {:data {:values raw}
          :vconcat [{:encoding {:x {:field "move" :type "quantitative"}
                                :y {:field "winrate" :type "quantitative"}}
                     :mark {:type "line" :point true :tooltip {:content "data"}}  :width w :title "winrate"}]})]
       [( kind/vega-lite (vl-choices (filter black? tcs) w "Black's scoremean values"))]
       [(kind/vega-lite (vl-choices (filter white? tcs) w "White's scoremean values"))]
       [(kind/vega-lite (vl-all-scoremeans (filter black? all-sm) w
                                           "Black's all scoreMeans for variations"))]
       [(kind/vega-lite (vl-all-scoremeans (filter white? all-sm) w
                                           "White's all scoreMeans for variations"))]
       [(kind/vega-lite
         (vl-bars-per-move effs-dat "effect" w "Effects of moves"))]
       [(kind/vega-lite
         (vl-bars-per-move white-effs-dat "effect" w "Effects of White's moves"))]
       [(kind/vega-lite
         (vl-bars-per-move black-effs-dat "effect" w "Effects of Black's moves"))]
       [(kind/vega-lite
         (vl-bars-per-move (normalize-effects (filter white? effs-dat)) "cumsum" w
                           "Cumulative moving average of effects (white)"))]
       [(kind/vega-lite
         (vl-bars-per-move (normalize-effects  (filter black? effs-dat)) "cumsum" w
                           "Cumulative moving average of effects (black)"))]
       [(kind/vega-lite
         (vl-bars-per-move (concat (normalize-effects (filter white? effs-dat))
                                   (normalize-effects (filter black? effs-dat)))
                           "cumsum" w
                           "Cumulative moving average of effects (composite)"))]
       [(kind/vega-lite
         (vl-bars-per-move (deviations white-effs-dat) "deviation" w
                           "Deviations from meean effect (white)"))]
       [(kind/vega-lite
         (vl-bars-per-move (deviations black-effs-dat) "deviation" w
                           "Deviations from mean effect (black)"))]
       [(vl-boxplot-summary effs-dat "effect" "Summary of effects")]
        [(when-not (empty? copd)
           (vl-boxplot-summary copd "cop" "Summary of cost of passings"))]
       ]})))

(defn game-report
  "Starts the clay server and sends the generated game report to the
   browser to display."
  [raw title]
  (clay/start!)
  (clay/handle-value! (create-game-report raw title)))

;; (defn cop-fingerprints
;;   [RAWs]
;;   [:div
;;    [:p "LambdaGo v" (version/get-version "lambdago" "lambdago")]
;;    [:h1 "Cost of passing fingerprints"]
;;    (for [[name RAW] RAWs]
;;      (let [copd (reduce
;;                  (fn [d m]
;;                    (if (= "black" (:color m))
;;                      (conj d m)
;;                      (conj d (update m :cop sign-swap))))
;;                  []
;;                  (cost-of-passing RAW))
;;            s (apply + (map :cop copd))
;;            m (count copd)]
;;        [:vega-lite
;;         (oz-bars-per-move copd "cop" (* 4.5 (count copd))
;;                           (str
;;                            name ", "
;;                            (format  "total: %.2f" (double s))
;;                            " for " m  " moves,"
;;                            (format "average: %.2f"  (double (/ s m)))))]))])
