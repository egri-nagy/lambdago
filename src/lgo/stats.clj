(ns lgo.stats
  "Statistical functions.")

(defn mean
  "Calculating the average."
  [nums]
  (/ (apply + nums)
     (count nums)))

(defn cmas
  "Cumulative moving averages. These can be calculated incrementally:
  when receiving a new value (the nth) we can scale the difference between
  the previvous cma and the new value by 1/n, and add it to the previous cma."
  [nums]
  (rest ;to drop the starting zero
   (reductions (fn [cma [x n]]
                 (+ cma (/ (- x cma) n)))
               0
               (map vector nums
                    (rest (range)))))) ;1,2,3,...

(defn median
  "Calculating the median for a collection of numerical values."
  [nums]
  (let [ordered (vec (sort nums))
        n (count nums)
        h (dec (int (/ n 2)))
        indices (if (odd? n)
                  [(inc h)]
                  [h (inc h)])]
    (mean (map ordered indices))))

(defn normalize
  "Normalizing the given collection of nonnegative numbers, so their sum is 1."
  [nums]
  (let [sum (apply + nums)]
    (map (fn [x] (/ x sum))
         nums)))

(defn KL-divergence
  "The Kullback-Leibler divergence of probability distributions P and Q.
  The information gain when using Q instead of P. Using natural logarithm."
  [P Q]
  (apply +
         (map
          (fn [p q]
            (* p (Math/log (/ p q))))
          P Q)))
