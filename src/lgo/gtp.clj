(ns lgo.gtp
  "The GTP engine of LambdaGo."
  (:require [clojure.string :refer [split]]
            [lgo.board :refer [empty-board put-stone]]
            [trptcolin.versioneer.core :as version]))

(def list-commands ["name" "version" "protocol_version" "boardsize" "clear_board"])

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
                "version" (do
                            (println "= "
                                     (version/get-version "lambdago"
                                                          "lambdago") "\n")
                            board)
                "name" (do
                         (println "= LambdaGo\n")
                            board)
                "protocol_version" (do
                                     (println "= 2\n")
                                     board)
                "list_commands" (do
                                  (print "=")
                                  (doseq [cmd list-commands]
                                    (println cmd))
                                  (println)
                                  board)
                "boardsize" (do
                              (println "=")
                              (let [n (read-string (second pieces))]
                                (empty-board n n)))
                "clear_board" (do
                              (println "=")
                              (let [n (:width board)] ;TODO what if the board is not initialized yet
                                (empty-board n n)))
                "genmove" (do
                            (println "=")
                            (let [n (read-string (second pieces))]
                              (empty-board n n)))
                board)]
          (recur (read-line) nboard))))))
