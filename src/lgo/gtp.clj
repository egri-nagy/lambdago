(ns lgo.gtp
  "The GTP engine of LambdaGo.
  The game is represented as a hash-map with keys :board, :moves, :history."
  (:require [clojure.string :refer [split]]
            [lgo.board :refer [empty-board put-stone]]
            [trptcolin.versioneer.core :as version]))

(def list-commands ["boardsize"
                    "clear_board"
                    "komi"
                    "name"
                    "protocol_version"
                    "version"
                    ])

(defn gtp-loop
  []
  (loop [input (read-line)
         game {}]
    (let [pieces (split input #" ")
          command (first pieces)]
      (if (= "quit" command)
        game
        (let [ngame
              (case command
                "boardsize" (do ; creating a new board with the given size
                              (println "= \n")
                              (let [n (read-string (second pieces))]
                                (conj game [:board (empty-board n n)])))
                "clear_board" (do
                                (println "= \n")
                                (let [n (:width (:board game))] ;TODO what if the board is not initialized yet
                                  (conj game [:board (empty-board n n)])))
                "version" (do
                            (println "= "
                                     (version/get-version "lambdago"
                                                          "lambdago") "\n")
                            game)
                "komi" (do ;komi is ignored for the moment
                         (println "= \n")
                         game)
                "name" (do
                         (println "= LambdaGo\n")
                         game)
                "protocol_version" (do
                                     (println "= 2\n")
                                     game)
                "list_commands" (do
                                  (print "= ")
                                  (doseq [cmd list-commands]
                                    (println cmd))
                                  (println)
                                  game)
               "genmove" (do
                            (println "=")
                            (let [n (read-string (second pieces))]
                              (empty-board n n)))
                game)] ;default
          (recur (read-line) ngame))))))
