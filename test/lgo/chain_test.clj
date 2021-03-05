(ns lgo.chain-test
  (:require [clojure.test :refer :all]
            [lgo.chain :refer :all]))

;; TODO: the test compare squences instead of sets, it works for now

(deftest inside-vs-boundary-test
  (testing "Testing the inside and boundary stones."
    (is (= (inside-stones  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
           '([2 2])))
    (is (= (inside-stones  [[1 2] [3 2] [2 1] [2 3] ] 3 3)
           '()))
    (is (= (boundary-stones  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
           '([1 2] [3 2] [2 1] [2 3])))))

(deftest envelope-test
  (testing "Testing the envelope."
    (is (= (envelope  [[1 2] [3 2] [2 1] [2 3] [2 2]] 3 3)
           '( [3 3] [1 1] [1 3] [3 1] )))
    (is (= (envelope  [[1 2] [3 2] [2 1] [2 3] ] 3 3)
           '([2 2]  [3 3] [1 1] [1 3] [3 1])))))
