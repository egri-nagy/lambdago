(ns lgo.board
  "Functions for representing board state and its evolution."
  (:require [lgo.util :refer [vec-rm]]
            [kigen.position :refer [index]]
            [clojure.set :refer [union]]))

;; The board position is stored as a vector of chains, in the order of creation.
;; A chain is represented by its oldest stone.
;; When connecting the newer chain is merged to the older one.


(def symbols {:b \X :w \O nil \.})

(def opposite {:b :w, :w :b})

;; a chain is a hash-map , the stones' order is not guaranteed since connecting could happen
;; internally we use numbers for coordinates
{:player :b, :stones [[3 4] [4 4]] :liberties [[][]]}

;;board position is a vector of chains + zobrist hash + whose move is that

(declare put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board position, ko?
         )
;;empty board


(defn neighbours
  "Neighbours of a grid point considering the size of the board,
  i.e. edges and corners are handled.
  We generate all, then filter the valid ones (not the fastest)."
  [[column row :as point] width height]
  (let [points  [[(dec column) row]
                 [(inc column) row]
                 [column (dec row)]
                 [column (inc row)]]]
    (filterv (fn [[c r]] (and (<= 1 c width)
                              (<= 1 r height)))
             points)))

(defn containing-chain
  "Returns the chain containing the given point.
  It can be used for retrieving touching chains by calling it for
  the neighbours of a grid point.
  There can be only one chain, as a point is occupied by one only, or nil.
  TODO: replace this with a reverse map (points to chains)."
  [{chains :chains} [column row :as point]]
  (first
   (filter
    (fn [chain] (let [stones (:stones chain)]
                  (not (empty? (filter (partial = point) stones)))))
    chains)))

(defn self-capture?
  "Decides whether placing a stone results in self-capture or not."
  [{width :width height :height chains :chains :as board}
   [column row :as point]
   color]
  )

(defn put-stone
  "Places a single stone  on the board, updating the chain list.
  For now this is used only for chain analysis, will not check legality of the
  move fully.
  The following things can happen to adjacent points:
  1. if it's empty, it becomes a liberty
  2. when occupied by enemy stones, that is
    a. captured if the point is its last liberty
    b. needs to be updated by removing the point from its liberties
  3. when occupied by a friendly stone, chains
    a. get merged
    b. possibly captured."
  [{width :width height :height chains :chains :as board}
   [column row :as point]
   color]
  (if (empty? (containing-chain board point))
    ;;the stone is not on the board yet
    (let [adjpts (neighbours point width height) ;;adjacent points, neighbours
          connected-chains (remove nil? (map (partial containing-chain board) adjpts))
          friendly-chains (filter (fn [chain] (= color (:player chain)))
                                  connected-chains)
          opponent-chains (filter (fn [chain] (= (opposite color) (:player chain)))
                                  connected-chains)
          ;;newer way of doing it
          chain_lookup (into {}
                             (map (fn [pt] [pt (containing-chain board pt)])
                                  adjpts))]
      (cond
        (empty? connected-chains) ;; an individual stone, new chain
        (update board :chains
                (fn [chains] (conj chains {:player color
                                           :stones [point]
                                           :liberties (set adjpts)})))

        (= 1 (count friendly-chains)) ;; a single friendly chain
        (-> board
            (update-in [:chains (index chains (first friendly-chains)) :stones]
                       (fn [v] (conj v point)))
            (update-in [:chains (index chains (first friendly-chains)) :liberties]
                       (fn [s] (union ))))))
    ;;illegal move, it's on the board already
    (do
      (println point " already on board")
      board)))

(def empty-board {:width 3 :height 3 :chains []})
(def single-stone-to-append
  (-> empty-board
      (put-stone [1 2] :b)))
(def two-stones-to-be-merged
  (-> empty-board
      (put-stone [1 2] :b)
      (put-stone [3 2] :b)))
