(ns lgo.bot.liberty
  "A simple bot playing random moves but not filling eyes and targeting
  the opponents low liberty number groups."
  (:require
   [lgo.board :refer [self-capture? put-stone opponent
                      board-string empty-points eye-fill?]]))

(defn candidate-moves
  "Liberty removing moves first, then random ones."
  [board color]
  (let [libs (vec
              (reduce into #{}
                      (filter #(<= (count %) 1) ;1 capture, 2 atari
                              (map (:liberties board)
                                   (filter #(= (opponent color) (:color %))
                                           (:chains board))))))
        extras (remove (set libs) (:empties board))]
    (concat (shuffle libs) (shuffle extras))))

(defn genmove
  "Given a board position, the color of the player to make a move, and
  a history of previous board positions, this returns a random legal
  non-eye-filling, low liberty targeting move,
  and the updated board position and the updated history."
  [{board :board history :history :as game}
   color]
  ;; Initially the candidates are all the empty points.
  (loop [cands (candidate-moves board color)]
    (if (empty? cands)
      (update game :moves #(conj % :pass)) ;passing if there are no options left
      (let [move (first cands)] ; just pick the first and try it
        ;; first some checking for illegal/bad moves
        (if (or (eye-fill? board color move)
                (self-capture? board color move))
          (recur (rest cands)) ;if the move is not good, just try the rest
          (let [nboard (put-stone board color move)
                current [(opponent color) (board-string nboard)]]
            (if (contains? history current)
              (recur (rest cands)) ;situational superko
              (-> game
                  (update :board (constantly nboard))
                  (update :moves #(conj % move))
                  (update :history #(conj % current))))))))))
