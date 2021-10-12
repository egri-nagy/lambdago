(ns lgo.grid-test
  (:require [clojure.test :refer :all]
            [lgo.grid :refer :all]))

(deftest neighbours-test
  (testing "Testing the neighbours function for edges and corners."
    (is (= (set (neighbours [1 1] 4 4))
           (set [[2 1] [1 2]] )))
    (is (= (set (neighbours [2 3] 4 4))
           (set [[1 3] [3 3] [2 2] [2 4]])))
    (is (= (set (neighbours [2 4] 4 4))
           (set [[1 4] [3 4] [2 3]])))))

(deftest points-test
  (testing "Testing the all points function."
    (is (= (points 1 1)
           [[1 1]] ))
    (is (= (points 2 3)
           [[1 1] [2 1] [1 2] [2 2] [1 3] [2 3]]))))



;; (deftest inside-vs-boundary-test
;;   (testing "Testing the neighbours function for edges and corners."
;;     (is (= (inside-points  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
;;            '([2 2])))
;;     (is (= (boundary-points  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
;;            '([1 2] [3 2] [2 1] [2 3])))))
