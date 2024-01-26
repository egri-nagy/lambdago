(ns lgo.board-test
  (:require [clojure.test :refer :all]
            [lgo.board :refer :all]))

(deftest board-2x1-tests
  (testing "Testing sequences of moves on the 2x1."
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone :b [1 1])
                             (put-stone :b [2 1])))
           "..\n"))
    (is (= (board-string (-> (empty-board 2 1)
                             (put-stone :b [1 1])
                             (put-stone :w [2 1])))
           ".O\n"))
    (is (nil? (-> (empty-board 2 1)
                  (put-stone :b [1 1])
                  (put-stone :w [1 1]))))))

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

(deftest capturing-connecting-test
  (let [bp [".OXO."
            "OOXOO"
            "XX.XX"
            "OXOOX"
            "OXXX."]
        b (build-position bp)
        bb (put-stone b :b [3 3])]
    (= (board-string bb)
       ".OXO.\nOOXOO\nXXXXX\nOX..X\nOXXX.\n")
    (= 4 (count (:chains bb)))))

(def blood_vomiting_game_moves '([:b [3 16]] [:w [16 17]] [:b [17 4]] [:w [5 4]] [:b [15 3]] [:w [5 17]] [:b [17 15]] [:w [17 11]] [:b [17 9]] [:w [15 16]] [:b [9 17]] [:w [4 14]] [:b [5 16]] [:w [4 16]] [:b [4 15]] [:w [4 17]] [:b [3 15]] [:w [5 15]] [:b [6 16]] [:w [3 17]] [:b [2 17]] [:w [2 18]] [:b [3 13]] [:w [7 18]] [:b [8 16]] [:w [5 14]] [:b [2 16]] [:w [3 11]] [:b [4 12]] [:w [4 11]] [:b [5 12]] [:w [7 14]] [:b [3 18]] [:w [4 18]] [:b [2 19]] [:w [6 17]] [:b [7 15]] [:w [1 18]] [:b [8 14]] [:w [7 13]] [:b [8 13]] [:w [7 12]] [:b [5 11]] [:w [8 12]] [:b [4 9]] [:w [9 15]] [:b [8 15]] [:w [6 15]] [:b [3 8]] [:w [3 4]] [:b [12 17]] [:w [16 14]] [:b [7 3]] [:w [17 14]] [:b [4 3]] [:w [3 3]] [:b [5 3]] [:w [3 6]] [:b [9 12]] [:w [7 16]] [:b [14 17]] [:w [9 3]] [:b [6 4]] [:w [12 3]] [:b [2 7]] [:w [2 6]] [:b [1 6]] [:w [2 9]] [:b [4 10]] [:w [5 8]] [:b [6 10]] [:w [17 7]] [:b [15 9]] [:w [17 5]] [:b [16 4]] [:w [14 6]] [:b [15 11]] [:w [16 11]] [:b [15 12]] [:w [16 10]] [:b [18 14]] [:w [18 13]] [:b [17 17]] [:w [17 18]] [:b [18 18]] [:w [17 16]] [:b [18 17]] [:w [16 18]] [:b [18 16]] [:w [16 15]] [:b [16 16]] [:w [13 16]] [:b [17 13]] [:w [16 13]] [:b [14 16]] [:w [13 17]] [:b [13 15]] [:w [12 16]] [:b [12 15]] [:w [11 16]] [:b [14 15]] [:w [14 18]] [:b [15 18]] [:w [17 16]] [:b [13 18]] [:w [18 15]] [:b [16 9]] [:w [12 18]] [:b [14 19]] [:w [9 16]] [:b [10 18]] [:w [8 17]] [:b [10 14]] [:w [11 15]] [:b [11 17]] [:w [11 14]] [:b [10 13]] [:w [11 13]] [:b [9 11]] [:w [13 11]] [:b [13 13]] [:w [9 18]] [:b [10 17]] [:w [11 11]] [:b [13 9]] [:w [14 13]] [:b [13 12]] [:w [11 9]] [:b [12 10]] [:w [12 11]] [:b [11 10]] [:w [10 10]] [:b [11 12]] [:w [12 12]] [:b [12 13]] [:w [15 10]] [:b [13 7]] [:w [10 12]] [:b [10 11]] [:w [14 10]] [:b [14 9]] [:w [9 13]] [:b [9 14]] [:w [10 15]] [:b [11 12]] [:w [10 19]] [:b [11 19]] [:w [10 12]] [:b [9 13]] [:w [9 19]] [:b [11 12]] [:w [5 9]] [:b [5 10]] [:w [10 12]] [:b [8 18]] [:w [8 19]] [:b [11 12]] [:w [3 14]] [:b [2 14]] [:w [10 12]] [:b [7 19]] [:w [6 19]] [:b [11 12]] [:w [8 9]] [:b [9 10]] [:w [10 12]] [:b [12 19]] [:w [10 9]] [:b [8 10]] [:w [15 5]] [:b [11 7]] [:w [10 7]] [:b [7 9]] [:w [14 3]] [:b [10 6]] [:w [14 2]] [:b [18 5]] [:w [9 6]] [:b [10 5]] [:w [8 4]] [:b [6 5]] [:w [7 6]] [:b [6 6]] [:w [6 7]] [:b [7 7]] [:w [7 8]] [:b [8 7]] [:w [6 9]] [:b [7 10]] [:w [9 7]] [:b [8 8]] [:w [8 6]] [:b [9 9]] [:w [18 6]] [:b [16 5]] [:w [16 6]] [:b [12 5]] [:w [11 4]] [:b [1 4]] [:w [11 8]] [:b [17 6]] [:w [5 6]] [:b [4 5]] [:w [17 5]] [:b [12 4]] [:w [12 7]] [:b [11 3]] [:w [10 4]] [:b [17 6]] [:w [4 2]] [:b [5 2]] [:w [17 5]] [:b [10 2]] [:w [15 4]] [:b [9 2]] [:w [13 6]] [:b [17 6]] [:w [10 16]] [:b [11 18]] [:w [17 5]] [:b [12 6]] [:w [11 6]] [:b [17 6]] [:w [8 3]] [:b [17 5]] [:w [8 2]] [:b [16 7]] [:w [15 7]] [:b [15 6]] [:w [3 7]] [:b [2 8]] [:w [16 6]] [:b [16 8]] [:w [2 2]] [:b [4 1]] [:w [3 2]] [:b [6 8]] [:w [1 3]] [:b [5 7]] [:w [2 4]] [:b [15 2]] [:w [15 1]] [:b [15 6]] [:w [13 8]] [:b [18 10]] [:w [11 12]]))

;; yields this board position
;; ...X..........O....
;; .OOOX..OXX...OX....
;; O.OXX.XOO.XO.OX....
;; XOO.OX.O.OOX..OXX..
;; ...X.X...X.X..OXXX.
;; XOO.OXOOOXOXOOX.XO.
;; .XO.X.XXOO.OX.OXO..
;; .XX.OX.X..O.O..X...
;; .O.XOOX.XOO.XXXXX..
;; ...XXXXXXOXX.OOO.X.
;; ..OOX...XXOOO.XOO..
;; ...XX.OOXOOOX.X....
;; ..X...OXXXOXXO.OXO.
;; .XOOO.OXXXO....OOX.
;; ..XXOOXXOOOXXX.O.O.
;; .XXO..OXOOOOOXO.OX.
;; .XOOOO.OXXXXOX.OXX.
;; OOXO..O.OXX.X.XOOX.
;; .X...O.OOOXX.X.....

(deftest blood-vomiting-test
  (is (= (board-string
          (reduce
           (partial apply put-stone)
           (empty-board 19 19)
           blood_vomiting_game_moves))
         "...X..........O....\n.OOOX..OXX...OX....\nO.OXX.XOO.XO.OX....\nXOO.OX.O.OOX..OXX..\n...X.X...X.X..OXXX.\nXOO.OXOOOXOXOOX.XO.\n.XO.X.XXOO.OX.OXO..\n.XX.OX.X..O.O..X...\n.O.XOOX.XOO.XXXXX..\n...XXXXXXOXX.OOO.X.\n..OOX...XXOOO.XOO..\n...XX.OOXOOOX.X....\n..X...OXXXOXXO.OXO.\n.XOOO.OXXXO....OOX.\n..XXOOXXOOOXXX.O.O.\n.XXO..OXOOOOOXO.OX.\n.XOOOO.OXXXXOX.OXX.\nOOXO..O.OXX.X.XOOX.\n.X...O.OOOXX.X.....\n")))
