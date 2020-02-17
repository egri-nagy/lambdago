(ns lgo.board-test
  (:require [clojure.test :refer :all]
            [lgo.board :refer :all]))

(deftest board-2x1-tests
  (testing "Testing sequences of moves on the 2x1."
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone :b [1 1])
                             (put-stone :b [2 1])))
           "X.\n"))
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone :b [1 1])
                             (put-stone :w [2 1])))
           ".O\n"))
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone :b [1 1])
                             (put-stone :w [1 1])))
           "X.\n"))))

(deftest board-2x2-tests
  (testing "Testing sequences of moves on the 2x2."
    (is (= (board-string (-> (empty-board 2 2)
                             (put-stone :b [1 1])
                             (put-stone :w [1 2])
                             (put-stone :b [2 2])))
           "X.\n.X\n"))
    (is (= (board-string (-> (empty-board 2 2)
                             (put-stone :b [1 1])
                             (put-stone :w [2 2])
                             (put-stone :b [1 2])
                             (put-stone :w [2 1])))
           ".O\n.O\n"))))

(deftest board-5x5-tests
  (let [game (-> (empty-board 5 5)
                 (put-stone :b [3 4]) (put-stone :w [3 3])
                 (put-stone :b [4 3]) (put-stone :w [4 4])
                 (put-stone :b [4 5]) (put-stone :w [2 4])
                 (put-stone :b [5 4]) (put-stone :w [3 2])
                 (put-stone :b [2 5]) (put-stone :w [1 4])
                 (put-stone :b [4 2]) (put-stone :w [3 1])
                 (put-stone :b [1 2]) (put-stone :w [2 2])
                 (put-stone :b [3 5]) (put-stone :w [1 3])
                 (put-stone :b [4 1]) (put-stone :w [1 1])
                 (put-stone :b [1 5]))]
    ;;final board position
    ;; O.OX.
    ;; .OOX.
    ;; O.OX.
    ;; OOX.X
    ;; XXXX.
    (is (= (board-string game)
           "O.OX.\n.OOX.\nO.OX.\nOOX.X\nXXXX.\n"))
    (is (= 6 (count (:chains game))))))
