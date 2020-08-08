(ns lgo.stats
  "Statistical functions.")

(defn mean
  "Calculating the average."
  [nums]
  (/ (apply + nums)
     (count nums)))

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

(defn KL-divergence
  "The Kullback-Leibler divergence of probability distributions P and Q.
  The information gain when using Q instead of P."
  [P Q]
  (apply +
         (map
          (fn [p q]
            (* p (Math/log (/ p q))))
          P Q)))
