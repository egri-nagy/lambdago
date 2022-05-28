(ns lgo.stats-test
  (:require [clojure.test :refer :all]
            [lgo.stats :refer :all]
            [clojure.math :refer [pow round]]))

(defn precision-fn
  "n is the number of decimal digits to be kept."
  [n]
  (let [p (pow 10 n)]
    (fn [x]
      (double (/ (round (* p x)) p)))))

(def with-8-digits (precision-fn 8))

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
          Q [1/3 1/3 1/3]
          P1 [0.25, 0.33, 0.23, 0.19]
          Q1 [0.21, 0.21, 0.32, 0.26]]
      (is (zero? (KL-divergence P P)))
      (is (= (with-8-digits (KL-divergence P Q))
             (with-8-digits 0.0852996013183706)))
      (is (= (with-8-digits (KL-divergence Q P))
             (with-8-digits 0.09745500678538754)))
      (is (= (with-8-digits (KL-divergence P1 Q1))
             (with-8-digits 0.057192913458712795)))
      (is (= (with-8-digits (KL-divergence Q1 P1))
             (with-8-digits 0.05569721781445004))))))
