(ns lgo.gtp
  "The GTP engine of LambdaGo.
  The game is represented as a hash-map with keys :board, :moves, :history."
  (:require [clojure.string :refer [split trim]]
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

(defn gtp-loop
  []
  (loop [input (read-line)
         game {:moves [] :history #{}}]
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
                                  {:moves [] :history #{} :board (empty-board n n)}))
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
                "play" (do
                         (let [col  (m (trim (str (read-string (second pieces)))))
                               gtpmove (nth pieces 2)
                               mm  (zipmap "ABCDEFGHJKLMNOPQRST" (range 1 25))
                               x (mm (first gtpmove))
                               y (read-string (apply str (rest gtpmove)))
                               nboard (if (nil? x)
                                        (:board game)
                                        (put-stone (:board game) col [x,y]))]
                           (println "= \n")
                           (update game :board (constantly nboard))))
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
                            (let [col  (m (trim (str (read-string (second pieces)))))
                                  ng  (genmove game col)
                                  m (last (:moves ng))
                                  mm  (zipmap (range 1 25) "ABCDEFGHJKLMNOPQRST")
                                  out (if (= :pass m)
                                        "PASS"
                                        (str (mm (first m)) (last m)))]
                              (println "= " out "\n")
                             ng))
                game)] ;default
          (recur (read-line) ngame))))))
