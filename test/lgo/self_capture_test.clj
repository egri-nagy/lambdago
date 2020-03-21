(ns lgo.self-capture-test
  (:require [clojure.test :refer :all]
            [lgo.board :refer :all]))


(deftest self-capture-test
  (let [position (build-position [".X."
                                  "X.X"
                                  "..."])
        eps (empty-points position)]
    (is (= 6 (count eps)))
    (is (= [[1 1] [3 1] ]
           (filterv (partial self-capture? position :w) eps)))))

(deftest self-capture-test2
  (let [position (reduce
                  (partial apply put-stone)
                  (empty-board 3 3)
                  [[:b [2 1]] [:b [3 1]] [:b [3 2]]
                   [:w [1 1]] [:b [1 2]] [:w [2 3]] [:w [3 3]]])]
    (is (= [[2 2] [1 3] ]
           (filterv (partial self-capture? position :w) [[2 2] [1 3]])))))
