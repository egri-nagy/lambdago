(ns lgo.oz-test
  (:require [clojure.test :refer :all]
            [lgo.analysis.oz :refer :all]))

(def data
  [{:color "black",
    :choice -0.860119477,
    :move 0,
    :average -3.6285533713204434,
    :median -2.13469791,
    :AI -0.864857765}
   {:color "white",
    :choice 0.869360556,
    :move 1,
    :average 0.12516472948085097,
    :median 0.176785937,
    :AI 0.860119477}] )

(deftest a-test
  (testing "Testing data transformation for Oz."
    (is (= (set (data-transform data))
           (set (data-transform2
                 data
                 [:color :move]
                 [:choice :median :AI :average]
                 :scoreMean))))))
