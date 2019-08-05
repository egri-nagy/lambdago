(ns lgo.board-test
  (:require [clojure.test :refer :all]
            [lgo.board :refer :all]))

(deftest neighbours-test
  (testing "Testing the neighbours function for edges and corners."
    (is (= (neighbours [1 1] 4 4) '([2 1] [1 2]) ))
    (is (= (neighbours [2 3] 4 4) '([1 3] [3 3] [2 2] [2 4])))
    (is (= (neighbours [2 4] 4 4) '([1 4] [3 4] [2 3])))))
