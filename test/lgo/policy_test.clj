(ns lgo.policy-test
  (:require [clojure.test :refer :all]
            [lgo.analysis.policy :refer :all]))

(deftest policy-table-indices
  (testing "Testing the policy table index GTP move conversion."
    (is (every?
         (fn [x] (= x (policy-table-index (GTP-move x))))
         (range 362)))))
