(ns lgo.board-test
  (:require [clojure.test :refer :all]
            [lgo.board :refer :all]))

(deftest board-2x1-tests
  (testing "Testing sequences of moves on the 2x1."
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone [1 1] :b)
                             (put-stone [2 1] :b)))
           "X.\n"))
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone [1 1] :b)
                             (put-stone [2 1] :w)))
           ".O\n"))
    ))

(deftest board-2x2-tests
  (testing "Testing sequences of moves on the 2x2."
    (is (= (board-string (-> (empty-board 2 2)
                             (put-stone [1 1] :b)
                             (put-stone [1 2] :w)
                             (put-stone [2 2] :b)))
           "X.\n.X\n"))))
