(ns lgo.gtp
  "The GTP engine of LambdaGo.
  The game is represented as a hash-map with keys :board, :moves, :history."
  (:require [clojure.string :refer [split trim join]]
            [lgo.board :refer [empty-board put-stone]]
            [trptcolin.versioneer.core :as version]
            [lgo.bot.random :refer [genmove]]))

(def list-commands ["boardsize"
                    "clear_board"
                    "genmove"
                    "komi"
                    "name"
                    "play"
                    "protocol_version"
                    "version"
                    ])

(def m {"B" :b "W" :w})

(def numcoord (zipmap "ABCDEFGHJKLMNOPQRSTUVWXYZ" (range 1 25)))
(def GTPcoord (zipmap (range 1 25) "ABCDEFGHJKLMNOPQRSTUVWXYZ"))

(defn gtp-loop
  "The main loop for listening to and executing GTP commands."
  []
  (loop [input (read-line) ;when starting, just read the first command and
         game {:moves [] :history #{}}] ;create an empty game
    (let [pieces (split input #" ") ;space is a separator for GTP commands
          command (first pieces)] ;the first one is the command
      (if (= "quit" command)
        game ;when done, we return the game object
        (let [ngame ;we create the next state, it might be the same
              (case command
                "boardsize" (do ; creating a new board with the given size
                              (println "= \n")
                              (let [n (read-string (second pieces))] ;getting the size
                                (conj game [:board (empty-board n n)])))
                "clear_board" (do
                                (println "= \n")
                                (let [n (:width (:board game))] ;TODO what if the board is not initialized yet
                                  {:moves [] :history #{} :board (empty-board n n)}))
                "version" (do
                            (println "= " (version/get-version "lambdago" "lambdago") "\n")
                            game)
                "komi" (do ;komi is ignored for the moment
                         (println "= \n")
                         game)
                "name" (do
                         (println "= LambdaGo\n")
                         game)
                "play" (let [col  (m (trim (str (read-string (second pieces)))))
                             gtpmove (nth pieces 2)
                             x (numcoord (first gtpmove))
                             y (read-string (join (rest gtpmove)))
                             nboard (if (nil? x)
                                      (:board game)
                                      (put-stone (:board game) col [x,y]))]
                         (println "= \n")
                         (update game :board (constantly nboard)))
                "protocol_version" (do
                                     (println "= 2\n")
                                     game)
                "list_commands" (do
                                  (print "= ")
                                  (doseq [cmd list-commands]
                                    (println cmd))
                                  (println)
                                  game)
               "genmove" (let [col  (m (trim (str (read-string (second pieces)))))
                               ng  (genmove game col)
                               m (last (:moves ng))
                               out (if (= :pass m)
                                     "PASS"
                                     (str (GTPcoord (first m)) (last m)))]
                           (println "= " out "\n")
                           ng)
                game)] ;default
          (recur (read-line) ngame))))))
