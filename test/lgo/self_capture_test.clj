(ns lgo.self-capture-test
  (:require [clojure.test :refer :all]
            [lgo.board :refer :all]))


(deftest self-capture-test-3x3
  (let [position (build-position [".X."
                                  "X.X"
                                  "..."])
        eps (empty-points position)]
    (is (= 6 (count eps)))
    (is (= [[1 1] [3 1] ]
           (filterv (partial self-capture? position :w) eps)))))

(deftest self-capture-test-3x3-fullboard
  (let [position (build-position ["XXX"
                                  "XXX"
                                  "XX."])]
    (is (= false (boolean (self-capture? position :w [3 3]))))
    (is (= true (boolean (self-capture? position :b [3 3]))))))
