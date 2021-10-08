(ns lgo.bot.random
  "A simple bot playing random moves including eye-fills."
  (:require
   [lgo.board :refer [self-capture? put-stone opposite
                      board-string empty-points]]))

(defn genmove
  "Given a board position, the color of the player to make a move, and
  a history of previous board positions, this returns a random legal move,
  and the updated board position and the updated history.
  Assumption is that we do it right, so there will be no need for rollback."
  [{board :board history :history :as game}
   color]
  ;; Initially the candidates are all the empty points.
  (loop [cands (shuffle (empty-points board))]
    (if (empty? cands)
      (update game :moves #(conj % :pass)) ;passing if there are no options left
      (let [move (first cands)] ; just pick the first and try it
        ;; first some checking for illegal/bad moves
        (if (self-capture? board color move)
          (recur (rest cands)) ;if the move is not good, just try the rest
          (let [nboard (put-stone board color move)
                current [(opposite color) (board-string nboard)]]
            (if (contains? history current)
              (recur (rest cands)) ;situational superko
              (-> game
                  (update :board (constantly nboard))
                  (update :moves #(conj % move))
                  (update :history #(conj % current))))))))))
