(ns lgo.bot.random
  "A simple bot playing random moves including eye-fills.
  It only passes when there are no legal moves left."
  (:require
   [lgo.board :refer [self-capture? put-stone opposite
                      board-string empty-points]]))

(defn candidate-moves
  "Initially the candidates are all the empty points.
  We split the collection at a random point and put together the other way
  around. No need to shuffle the whole collection."
  [board]
  (let [empty-pts (empty-points board)
        k (rand-int (count empty-pts))]
    (concat (drop k empty-pts) (take k empty-pts))))

(defn genmove
  "Given a board position and the color of the player to make a move,
  this makes a random legal move, and returns an updated board,
  including the move sequence and history."
  [{board :board history :history :as game}
   color]
  (loop [cands (candidate-moves board)]
    (if (empty? cands)
      (update game :moves #(conj % :pass)) ;passing if there are no options left
      (let [move (first cands)] ; just pick the first and try it
        ;; first checking legality
        (if (self-capture? board color move)
          (recur (rest cands)) ;if the move is not good, just try the rest
          (let [nboard (put-stone board color move)
                current [(opposite color) (board-string nboard)]]
            (if (contains? history current)
              (recur (rest cands)) ;situational superko, another exit point
              (-> game
                  (update :board (constantly nboard))
                  (update :moves #(conj % move))
                  (update :history #(conj % current))))))))))
