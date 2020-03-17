(ns lgo.bot.random
  "A simple bot playing random moves."
  (:require
   [lgo.grid :refer [points]]
   [lgo.board :refer [legal-move?]]))

(defn genmove
  [{lookup :lookup width :width height :height :as board}
   color
   history]
  (let [pts (points width height)
        empty_pts (filter (comp nil? lookup) pts)
        candidates (shuffle empty_pts)
        move (first (filter (partial legal-move? board color history) candidates))]
    (if (nil? move)
      :pass
      move)))
