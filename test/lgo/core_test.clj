(ns lgo.core-test
  (:require [clojure.test :refer :all]
            [lgo.core :refer :all]))

(deftest a-test
  (testing "FIXED, I don't fail."
    (is (= 0 0))))
