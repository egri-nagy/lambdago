(ns lgo.game
  "Playing a game."
  (:require
   [lgo.board :refer [empty-board put-stone]]
   [lgo.bot.random :refer [genmove]]))

(defn random-game
  [boardsize]
  (loop [board (empty-board boardsize boardsize)
         turns (cycle [:b :w])
         moves []
         history #{}]
    (if (and (= :pass (last moves))
             (= :pass (last (butlast moves))))
      [board moves]
      (let [color (first turns)
            move (genmove board color history)
            nboard (if (= :pass move)
                     board
                     (put-stone board color move ))
            nhistory (if (= :pass move)
                       history
                       (conj history [color nboard]))]
        (print history)
        (recur nboard
               (rest turns)
               (conj moves move)
               nhistory)))))
