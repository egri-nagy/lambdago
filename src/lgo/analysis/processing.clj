(ns lgo.analysis.processing
  " Design decisions:
  Internally all score values are from Black's perspective: positive means Black
  win, negative means White win.
  Move counter tells how many moves were made. Color tells whose turn is it.

  The raw data is a hash-map with the folowing keys.

  :color :move :mean :meanmean :medianmean :means

  The dat input refers to this database."
  (:require [lgo.stats :refer [mean cmas]]
            [meander.epsilon :refer [match]]))


(def sign-swap (partial * -1))

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

;; working with the database ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defn effects
  "The score mean differences caused by the moves."
  [dat]
  (map (fn [[{c1 :color m1 :mean} {m2 :mean v2 :move}]]
         {:color c1 ;the owner of the effect is the first color
          :effect (- m2 m1)
          :move v2})
       (partition 2 1 dat))) ; all pairs of the rows

;; working with effects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn deviations
  "Calculating the deviations of the effects from the average.
  This assumes that data is for the same color."
  [effs]
  (let [avg (mean (map :effect effs))] ;the average of all the effects
    (map (fn [{e :effect :as d}]
           (into d [[:deviation (- e avg)]])) ;adding :deviation to the map
         effs)))

;; cost of passing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn cost-of-passing
  [dat]
  (let [game (:game dat)
        passed (:passed dat)
        tuples (map vector
                    (map :color game) ;color
                    (map :mean game) ;mean
                    (map :mean passed) ;hypothetical passed mean
                    (map :mean (rest game)) ;skipping first to get next mean
                    (map :move game))] ;move number
    (map (fn [[col mean passedmean nextmean move]]
           (let [cop (- mean passedmean)
                 achievement (- nextmean passedmean)]
             {:color col
              :cop cop
              :efficiency (min (* 100 (/ achievement cop)) 151)
              :move move}))
         tuples)))

(defn cost-of-passing-data
  [RAW]
  (reduce
   (fn [d m]
     (if (= "black" (:color m))
       (conj d m)
       (conj d (update m :cop sign-swap))))
   []
   (cost-of-passing RAW)))

(defn effect-vs-cop
  [effd copd]
  (map
   (fn [{effcol :color effect :effect move :move}
        {copcol :color cop :cop}]
     (when (= effcol copcol)
       {:color effcol :effect effect :cop cop :move move}))
   effd copd))

(defn normalize-effects
  "assuming that it is from one player"
  [e-d]
  (let [avgs (cmas (map :effect e-d))]
    (map (fn [d v]
           (conj d [:cumsum v]))
         e-d avgs)))
