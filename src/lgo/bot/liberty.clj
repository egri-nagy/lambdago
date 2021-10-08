(ns lgo.bot.liberty
  "A simple bot playing random moves."
  (:require
   [lgo.board :refer [self-capture? put-stone opposite
                      board-string empty-points eye-fill?]]))

(defn genmove
  "Given a board position, the color of the player to make a move, and
  a history of previous board positions, this returns a random legal move,
  and the updated board position and the updated history.
  Assumption is that we do it right, so there will be no need for rollback."
  [{board :board history :history :as game}
   color]
  ;; Initially the candidates are all the empty points.
  (loop [cands (let [libs (vec (reduce into #{}
                           (filter #(< (count %) 2)
                                   (map (:liberties board)
                                        (filter #(= (opposite color) (:color %))
                                                (:chains board))))))
                     empties (set (empty-points board))
                     extras (remove (set libs) empties)]
           (concat (shuffle libs) (shuffle extras)))]
    (if (empty? cands)
      (update game :moves #(conj % :pass)) ;passing if there are no options left
      (let [move (first cands)] ; just pick the first and try it
        ;; first some checking for illegal/bad moves
        (if (or (eye-fill? board color move)
                (self-capture? board color move))
          (recur (rest cands)) ;if the move is not good, just try the rest
          (let [nboard (put-stone board color move)
                current [(opposite color) (board-string nboard)]]
            (if (contains? history current)
              (recur (rest cands)) ;situational superko
              (-> game
                  (update :board (constantly nboard))
                  (update :moves #(conj % move))
                  (update :history #(conj % current))))))))))
