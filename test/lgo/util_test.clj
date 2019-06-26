(ns lgo.util-test
  (:require [clojure.test :refer :all]
            [lgo.util :refer :all]))

(deftest vec-rm-test
  (testing "Testing 'removing' an element from a vector."
    (is (= (vec-rm [1 2 3] 1) [1 3]))
    (is (= (vec-rm [1 2 3] 2) [1 2]))
    (is (= (vec-rm [1 2 3] 0) [2 3]))))
