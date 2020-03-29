(ns lgo.gtp
  "The GTP engine of LambdaGo.")

(defn gtp-loop
  []
  (loop [input (read-line)]
    (when-not (= "x" input)
      (println input)
      (recur (read-line)))))
