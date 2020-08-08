(ns lgo.stats
  "Statistical functions."
  (:require [clojure.math.numeric-tower :as math]))

(defn mean
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
