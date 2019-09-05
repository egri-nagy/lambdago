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
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone [1 1] :b)
                             (put-stone [1 1] :w)))
           "X.\n"))))
    

(deftest board-2x2-tests
  (testing "Testing sequences of moves on the 2x2."
    (is (= (board-string (-> (empty-board 2 2)
                             (put-stone [1 1] :b)
                             (put-stone [1 2] :w)
                             (put-stone [2 2] :b)))
           "X.\n.X\n"))
    (is (= (board-string (-> (empty-board 2 2)
                             (put-stone [1 1] :b)
                             (put-stone [2 2] :w)
                             (put-stone [1 2] :b)
                             (put-stone [2 1] :w)))
           ".O\n.O\n"))))

(deftest board-5x5-tests
  (let [game (-> (empty-board 5 5)
                 (put-stone [3 4] :b) (put-stone [3 3] :w)
                 (put-stone [4 3] :b) (put-stone [4 4] :w)
                 (put-stone [4 5] :b) (put-stone [2 4] :w)
                 (put-stone [5 4] :b) (put-stone [3 2] :w)
                 (put-stone [2 5] :b) (put-stone [1 4] :w)
                 (put-stone [4 2] :b) (put-stone [3 1] :w)
                 (put-stone [1 2] :b) (put-stone [2 2] :w)
                 (put-stone [3 5] :b) (put-stone [1 3] :w)
                 (put-stone [4 1] :b) (put-stone [1 1] :w)
                 (put-stone [1 5] :b))]
    ;;final board position
    ;; O.OX.
    ;; .OOX.
    ;; O.OX.
    ;; OOX.X
    ;; XXXX.
    (is (= (board-string game)
           "O.OX.\n.OOX.\nO.OX.\nOOX.X\nXXXX.\n"))
    (is (= 6 (count (:chains game))))))
