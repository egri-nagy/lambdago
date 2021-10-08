(ns lgo.bot.random
  "A simple bot playing random moves including eye-fills.
  It only passes when there are no legal moves left."
  (:require
   [lgo.board :refer [self-capture? put-stone opposite
                      board-string empty-points]]))

(defn genmove
  "Given a board position and the color of the player to make a move,
  this makes a random legal move, and returns an updated board,
  including the move sequence and history."
  [{board :board history :history :as game}
   color]
  ;; Initially the candidates are all the empty points.
  ;; shuffle is used to create a random sequence
  (loop [cands (shuffle (empty-points board))]
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
