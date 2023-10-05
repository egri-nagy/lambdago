(ns ^:skip-aot lgo.analysis.clay
  "Functions for generating reports by using clay."
 (:require [lgo.stats :refer [cmas]]
           [lgo.analysis.processing :refer [unroll-scoremeans
                                            effects
                                            choices
                                            deviations
                                            cost-of-passing
                                            effect-vs-cop
                                            normalize-effects]]
           [lgo.analysis.vl :refer [vl-bars-per-move]]
           [scicloj.clay.v2.api :as clay]
           [scicloj.kindly.v4.kind :as kind]))

(def sign-swap (partial * -1))

(defn create-game-report
  [RAW title]
  (let [raw (:game RAW) ;; the game entries
       
        ;;effects
        raw-effs (effects raw)
        white? #(= "white" (:color %))
        black? #(= "black" (:color %))
        black-effs-dat (filter black? raw-effs)
        white-effs-dat (map
                        (fn [d] (update d :effect sign-swap))
                        (filter white? raw-effs))
        effs-dat (concat black-effs-dat white-effs-dat)
    
        N (count effs-dat)
        w (int (* 5.4 N))
        h (int (* 2 N))]
    (kind/table
     {:column-names [title]
      :row-vectors
      [[(kind/vega-lite
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
                           "Deviations from mean effect (black)"))]]})))

(defn game-report
  [raw title]
  (clay/start!)
  (clay/handle-value! (create-game-report raw title)))