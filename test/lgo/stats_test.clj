(ns lgo.stats-test
  (:require [clojure.test :refer :all]
            [lgo.stats :refer :all]))

(deftest mean-test
  (testing "Testing mean."
    (is (= (mean [1 2 3]) 2))
    (is (= (mean [1 -1]) 0))
    (is (= (mean [2, 10, 21, 23, 23, 38, 38]) (/ 155 7)))))


(deftest cmas-test
  (testing "Testing cumulative moving averages."
    (is (= (cmas [1 2 3]) [1 3/2 2]))
    (is (= (cmas [1 -1]) [1 0]))
    (is (= (cmas [2, 10, 21, 23, 23, 38, 38]) [2 6 11 14 79/5 39/2 155/7]))))

(deftest median-test
  (testing "Testing median."
    (is (= (median [13]) 13))
    (is (= (median [1 2 3]) 2))
    (is (= (median [1 -1]) 0))
    (is (= (median [38, 10, 21, 23, 23, 38, 2]) 23))
    (is (== (median [38, 10, 21, 23, 22, 38, 2, 1]) 21.5))
    (is (= (median (range 10001) ) 5000))))

(deftest normalize-test
  (testing "Testing normalization."
    (is (= (normalize [1 0 0 0 0]) [1 0 0 0 0]))
    (is (= (normalize [1000 1000]) [1/2 1/2]))
    (is (= (normalize [1 0 0 0 2]) [1/3 0 0 0 2/3]))))


(deftest KL-divergence-test
  (testing "Testing Kullback-Leibler divergence."
    (let [P [0.36 0.48 0.16] ; Wikipedia example
          Q [1/3 1/3 1/3]]
      (is (= (KL-divergence P Q) 0.0852996013183706))
      (is (= (KL-divergence Q P) 0.09745500678538754)))))
