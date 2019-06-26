(ns lgo.sgf
  "Functions for working with the Smart Game Format"
  (:require [clojure.string :as string]))

(def letters "abcdefghijklmnopqrstuvwxyz")
(def noiletters "abcdefghjklmnopqrstuvwxyz")

(def letter->coord (zipmap letters
                           (range 1 (inc (count letters)))))

(def letter->noiletter (zipmap letters noiletters))

(defn sgfcoord->igocoord
  [cc]
  (str (letter->noiletter (first cc))
       (letter->coord (second cc))))

(defn sgfcoords->igocoords
  [s]
  (let [coords (re-seq #"[a-z]+" s)
        converted (map sgfcoord->igocoord coords)]
    (string/join "," converted)))

(defn extract-black
  [s]
  (str "\\black{"
       (sgfcoords->igocoords
        (first (re-find #"AB(\[[a-z]*\])*" s)))
       "}"))

(defn extract-white
  [s]
  (str "\\white{"
       (sgfcoords->igocoords
        (first (re-find #"AW(\[[a-z]*\])*" s)))
       "}"))

(defn extract
  [s]
  (str (extract-white s)
       (extract-black s)))

(defn convert [file]
  (println (extract (slurp file))))
