(ns lgo.board
  "Functions for representing board state and its evolution."
  (:require [lgo.util :refer [vec-rm]]))

;; The board position is stored as a vector of chains, in the order of creation.
;; A chain is represented by its oldest stone.
;; When connecting the newer chain is merged to the older one.


(def symbols {:b \X :w \O nil \.})

;; a chain is a hash-map , the stones' order is not guaranteed since connecting could happen
;; internally we use numbers for coordinates
{:player :b, :stones [[3 4] [4 4]]}

;;board position is a vector of chains + zobrist hash + whose move is that

(declare put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board position, ko?
         )
;;empty board
{:width 2 :height 2 :chains []}

(defn neighbours
  "Neighbours of a grid point considering the size of the board,
  i.e. edges and corners are handled.
  We generate all, then filter the valid ones (not the fastest)."
  [[column row :as point] width height]
  (let [points  [[(dec column) row]
                 [(inc column) row]
                 [column (dec row)]
                 [column (inc row)]]]
    (filter (fn [[c r]] (and (<= 1 c width)
                             (<= 1 r height)))
            points)))

(defn containing-chain
  [board [column row :as point]]
  (filter
   (fn [chain] (let [stones (:stones chain)]
                 (not (empty? (filter (partial = point) stones)))))
   board))

(defn put-stone
  "Places a single stone  on the board, updating the chain list."
  [board [[column row] :as point] color]
  (if (empty? (containing-chain board point))

    (do
      (println point " already on board")
      board))
  )
