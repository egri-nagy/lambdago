(ns lgo.analysis.processing
  " Design decisions:
  Internally all score values are from Black's perspective: positive means Black
  win, negative means White win.
  Move counter tells how many moves were made. Color tells whose turn is it.

  The raw data is a hash-map with keys color, move, mean, meanmean, medianmean,
  means. The dat input refers to this database."
  (:require [lgo.stats :refer [mean]]))

;;TODO we repeatedly switch sign for white, is there a better place to do that?

(defn unroll-scoremeans
  "All score means from raw data. This is just unrolling the means vector
  into separate rows."
  [dat]
  (mapcat
   (fn [d]
     (for [m (:means d)]
       {:color (:color d)
        :move (:move d)
        :mean (if (= "black" (:color d))
                m
                (- m))})) ; to get the value form white's perspective
   dat))

(defn effects
  "The score mean differences caused by the moves."
  [gamedat]
  (map (fn [[{c1 :color m1 :mean}
             {m2 :mean v2 :move}]]
         (let [eff (if (= c1 "black") ; need to negate for White
                     (- m2 m1)
                     (- (- m2 m1)))]
           {:color c1 :effect eff :move v2}))
       (partition 2 1 gamedat))) ; all pairs of the rows

(defn effects-with-cost-of-passing
  "The score mean differences caused by the moves."
  [effs cops]
  (map (fn [{c :color m :move e :effect }
            {cop :cop}]
         {:color c :effect (/ e cop) :move m})
       effs
       cops))

(defn choices
  [dat]
  (let [ps (partition 2 1 dat)]
    (map (fn [[{c1 :color  m1 :mean mm :meanmean md :medianmean v1 :move}
               {m2 :mean}]]
           (if (= "black" c1)
             {:color c1 :choice m2 :move v1 :average mm :median md :AI m1}
             {:color c1 :choice (- m2) :move v1 :average (- mm) :median (- md) :AI (- m1)}
             ))
         ps)))

(defn deviations
  "Calculating the deviations of the effects from the average."
  [effs]
  (let [avg (mean (map :effect effs))] ;the average of all the effects
    (map (fn [{e :effect :as d}]
           (into d [[:deviation (- e avg)]])) ;adding :deviation to the map
         effs)))

(defn cost-of-passing
  [dat]
  (let [tuples
        (map vector
             ((comp (partial map :color) :game) dat) ;color
             ((comp (partial map :mean) :game) dat) ;mean
             ((comp (partial map :mean) :passed) dat) ;hypothetical passed mean
             ((comp (partial map :move) :game) dat))] ;move number
    (map (fn [[c m pm move]]
           {:color c
            :cop (if (= "black" c)
              (- m pm)
              (- pm m))
            :move move})
         tuples)))

(defn efficiency
  [dat cops]
  (let [triples (map vector
                     ((comp (partial map :color) :game) dat)
                     ((partial map :mean)  (rest (:game dat)))
                     ((comp (partial map :mean) :passed) dat)
                     ((partial map :move)  (rest (:game dat))))
        realized (map (fn [[c m pm move]] ; is this the effect?
                        [c
                         (if (= "black" c)
                                (- m pm)
                                (- pm m))
                         move])
                      triples)]
    (map (fn [[c v m] cop]
           {:color c
            :cop (* 100 (/ v cop))
            :move m})
         realized
         (map :cop cops))))
