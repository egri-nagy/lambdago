(ns lgo.gtp
  "The GTP engine of LambdaGo.
  The game is represented as a hash-map with keys :board, :moves, :history."
  (:require [clojure.string :refer [split trim join]]
            [lgo.board :refer [empty-board put-stone]]
            [trptcolin.versioneer.core :as version]
            ;;engines
            [lgo.bot.random]
            [lgo.bot.liberty]))

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
  "The main loop for listening to and executing GTP commands. The engine
  parameter is used to find the appropriate genmove function."
  [engine]
  (let [;here is the LISP trickery to get the right function dynamically
        genmove (eval (symbol (str "lgo.bot." engine "/genmove")))]
    (loop [input (read-line) ;when starting, just read the first command and
           game {:moves [] :history #{}}] ;create an empty game
      (let [args (split input #" ") ;space is a separator in GTP commands
            command (first args)] ;the first argument is the command
        (if (= "quit" command)
          game ;when done, we return the game object
          (let [ngame ;we create the next state, it might be the same
                (case command ;;action selection based on commands

                  ;; GAME COMMANDS the most important/interesting part ;;;;;;;;;
                  ;; plays the move received through GTP
                  "play"
                  (let [color (m (trim (str (read-string (second args)))))
                        gtpmove (nth args 2)
                        x (numcoord (first gtpmove))
                        y (read-string (join (rest gtpmove)))
                        nboard (if (nil? x)
                                 (:board game)
                                 (put-stone (:board game) color [x,y]))]
                    (println "= \n")
                    (update game :board (constantly nboard)))
                  ;; generating a move
                  "genmove"
                  (let [color (m (trim (str (read-string (second args)))))
                        ng  (genmove game color)
                        m (last (:moves ng))
                        out (if (= :pass m)
                              "PASS"
                              (str (GTPcoord (first m)) (last m)))]
                    (println "= " out "\n")
                    ng)

                  ;; INITIALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; creates a new board of the given size for the current game
                  "boardsize"
                  (do
                    (println "= \n")
                    (let [n (read-string (second args))] ;getting the size
                      (conj game [:board (empty-board n n)])))
                  ;; clears the board (creates a new 19x19 if there is none)
                  "clear_board"
                  (do
                    (println "= \n")
                    (let [current_size (:width (:board game))
                          n (if-not current_size
                              current_size
                              19)]
                      {:moves [] :history #{} :board (empty-board n n)}))
                  ;; setting komi
                  "komi"
                  (do ;komi is ignored for the moment
                    (println "= \n")
                    game)

                  ;; ADMINISTRATIVE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; returns the version number information
                  "version"
                  (do
                    (println "= "
                             (version/get-version "lambdago" "lambdago")
                             engine"\n")
                    game)
                  ;; name of the bot
                  "name"
                  (do
                    (println "= LambdaGo " engine "\n")
                    game)
                  ;; GTP stuff?
                  "protocol_version"
                  (do
                    (println "= 2\n")
                    game)
                  ;; the list of implemented commands
                  "list_commands"
                  (do
                    (print "= ")
                    (doseq [cmd list-commands]
                      (println cmd))
                    (println)
                    game)

                  ;; default behavior - returning the unchanged game
                  game)]
            ;; keep reading the next line in an infinite loop
            (recur (read-line) ngame)))))))
