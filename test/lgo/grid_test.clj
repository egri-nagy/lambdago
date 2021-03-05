(ns lgo.grid-test
  (:require [clojure.test :refer :all]
            [lgo.grid :refer :all]))

(deftest neighbours-test
  (testing "Testing the neighbours function for edges and corners."
    (is (= (neighbours [1 1] 4 4)
           [[2 1] [1 2]] ))
    (is (= (neighbours [2 3] 4 4)
           [[1 3] [3 3] [2 2] [2 4]]))
    (is (= (neighbours [2 4] 4 4)
           [[1 4] [3 4] [2 3]]))))

;; (deftest inside-vs-boundary-test
;;   (testing "Testing the neighbours function for edges and corners."
;;     (is (= (inside-points  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
;;            '([2 2])))
;;     (is (= (boundary-points  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
;;            '([1 2] [3 2] [2 1] [2 3])))))
