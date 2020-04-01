(ns lgo.gtp
  "The GTP engine of LambdaGo."
  (:require [clojure.string :refer [split]]
            [lgo.board :refer [empty-board put-stone]]))

(defn gtp-loop
  []
  (loop [input (read-line)
         board nil]
    (let [pieces (split input #" ")
          command (first pieces)]
      (if (= "quit" command)
        board
        (let [nboard
              (case command
                "boardsize" (do
                              (println "=")
                              (let [n (read-string (second pieces))]
                                (empty-board n n)))
                "genmove" (do
                            (println "=")
                            (let [n (read-string (second pieces))]
                              (empty-board n n)))
                board)]
          (recur (read-line) nboard))))))
