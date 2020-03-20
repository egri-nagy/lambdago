(ns lgo.bot.random
  "A simple bot playing random moves."
  (:require
   [lgo.grid :refer [points]]
   [lgo.board :refer [self-capture? put-stone opposite board-string]]))

(defn genmove
  [{lookup :lookup width :width height :height :as board}
   color
   history]
  (let [pts (points width height)
        empty_pts (filter (comp nil? lookup) pts)
        candidates (shuffle empty_pts)]
    (println (count history) history "\n")
    (loop [cands candidates]
      (if (empty? cands)
        [:pass board]
        (let [move (first cands)]
          (if (self-capture? board color move)
            (recur (rest cands))
            (let [nboard (put-stone board color move)]
              (if (contains? history [(opposite color) (board-string nboard)])
                (recur (rest cands))
                [move nboard]))))))))
