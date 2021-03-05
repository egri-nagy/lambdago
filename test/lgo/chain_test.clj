(ns lgo.chain-test
  (:require [clojure.test :refer :all]
            [lgo.chain :refer :all]))

(deftest inside-vs-boundary-test
  (testing "Testing the neighbours function for edges and corners."
    (is (= (inside-stones  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
           '([2 2])))
    (is (= (inside-stones  [[1 2] [3 2] [2 1] [2 3] ] 3 3)
           '()))
    (is (= (boundary-stones  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
           '([1 2] [3 2] [2 1] [2 3])))))
