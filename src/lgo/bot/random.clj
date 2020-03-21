(ns lgo.bot.random
  "A simple bot playing random moves."
  (:require
   [lgo.board :refer [self-capture? put-stone opposite
                      board-string empty-points]]))

(defn genmove
  [board color history]
  (loop [cands (shuffle (empty-points board))]
    (if (empty? cands)
      [:pass board]
      (let [move (first cands)]
        (if (self-capture? board color move)
          (recur (rest cands))
          (let [nboard (put-stone board color move)]
            (if (contains? history [(opposite color) (board-string nboard)])
              (recur (rest cands))
              [move nboard])))))))
