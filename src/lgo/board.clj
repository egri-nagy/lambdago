(ns lgo.board
  "Functions for representing board state and its evolution."
  (:require [lgo.util :refer [vec-rm]]
            [kigen.position :refer [index]]))

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
  the neighbours of a grid point."
  [{chains :chains} [column row :as point]]
  (filter
   (fn [chain] (let [stones (:stones chain)]
                 (not (empty? (filter (partial = point) stones)))))
   chains))

(defn self-capture?
  "Decides whether placing a stone results in self-capture or not."
  [{width :width height :height chains :chains :as board}
   [column row :as point]
   color]
  )

(defn put-stone
  "Places a single stone  on the board, updating the chain list.
  For now this is used only for chain analysis, will not check legality of the
  move fully."
  [{width :width height :height chains :chains :as board}
   [column row :as point]
   color]
  (if (empty? (containing-chain board point))
    ;;the stone is not on the board yet
    (let [points (neighbours point width height)
          connected-chains (mapcat (partial containing-chain board) points)
          friendly-chains (filter (fn [chain] (= color (:player chain)))
                                  connected-chains)
          opponent-chains (filter (fn [chain] (= (opposite color) (:player chain)))
                                  connected-chains)]
      (cond
        (empty? connected-chains) ;; an individual stone
        (update board :chains
                (fn [chains] (conj chains {:player color :stones [point] :liberties points})))

        (= 1 (count friendly-chains)) ;; a single friendly chain
        (update-in board
                   [:chains (index chains (first friendly-chains)) :stones]
                   (fn [v] (conj v point)))
        ))
    ;;illegal move, it's on the board already
    (do
      (println point " already on board")
      board))
  )

(def empty-board {:width 3 :height 3 :chains []})
(def signle-stone-to-append
  (-> empty-board
      (put-stone [1 2] :b)))
(def two-stones-to-be-merged
  (-> empty-board
      (put-stone [1 2] :b)
      (put-stone [3 2] :b)))
