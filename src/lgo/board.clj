(ns lgo.board
  "Functions for representing board state and its evolution."
  (:require
   [lgo.grid :refer [neighbours]]
   [lgo.util :refer [vec-rm-all vec-rm]]
   [kigen.position :refer [index]]
   [clojure.set :refer [union difference]]))

;; The board position is stored as a vector of chains, in the order of creation.
;; A chain is represented by its oldest stone.
;; When connecting the newer chain is merged to the older one.


(def symbols {:b \X :w \O nil \.})

(def opposite {:b :w, :w :b})

;; a chain is a hash-map , the stones' order is not guaranteed since connecting could happen
;; internally we use numbers for coordinates
{:color :b, :stones [[3 4] [4 4]] :liberties [[][]]}

;;board position is a vector of chains + zobrist hash + whose move is that

(declare empty-board ;; data structure for an empty board
         put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board position, ko?
         merge-chains ;;merging friendly chains
         board-string ;; traditional ASCII rendering of the board
         )

(defn empty-board
  "Creates an empty board with the given dimensions."
  [width height]
  {:width width
   :height height
   :chains []
   :lookup {}}) ;; points to chains

(defn add-chain
  [board chain]
  ;;adding it to the list of chains
  (-> board
      (update :chains (fn [chains] (conj chains chain)))
      ;;updating the reverse lookup
      (update :lookup (fn [m] (conj m [(first (:stones chain)) chain])))))

(defn capture-chain
  [board chain]
  (-> board
      (update :chains
              (fn [chains] (vec-rm  chains
                                    (index chains chain))))
      (update :lookup
              (fn [m]
                (apply dissoc m (:stones chain))))))

(defn capture-chains
  [board ochains]
  (reduce capture-chain board ochains))

(defn update-chains
  [{chains :chains :as board} ochains point]
  (reduce (fn [brd chn]
            (let [chn_index (index chains chn)
                  brd2 (update-in brd
                                  [:chains chn_index :liberties]
                                  #(difference % #{point}))
                  chn2 (nth (brd2 :chains) chn_index)]
              (update brd2
                      :lookup
                      (fn [m] (into m (map (fn [pt] [pt chn2]) (:stones chn2)))))))
          board
          ochains))

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
  [{width :width height :height chains :chains lookup :lookup :as board}
   [column row :as point]
   color]
  (if (lookup point)
    ;;illegal move, it's on the board already
    (do
      (println point " already on board")
      board)
    ;;otherwise the stone is not on the board yet
    (let [opponent (opposite color)
          adjpts (neighbours point width height) ;;adjacent points, neighbours
          liberties (set (filter (complement lookup) adjpts))
          ;; adjacent chains, no duplicates
          adj_chains (filter identity (distinct (map lookup adjpts)))
          grouped_chains (group-by :color adj_chains)
          friendly_chains (grouped_chains color)
          opponent_chains (grouped_chains (opposite color))
          to_be_captured (filter #(= 1 (count (:liberties %))) opponent_chains)
          to_be_updated (remove (set to_be_captured) opponent_chains)
          updated_board (-> board
                            (capture-chains to_be_captured)
                            (update-chains to_be_updated point))]
      (println updated_board)
      ;; an individual stone, all neighbours are liberties
      (if (empty? friendly_chains)
        (if (= 1 (count (reduce union
                                (union liberties #{point})
                                (map :liberties friendly_chains))))
          (do
            (println friendly_chains point "self-capture")
            board)
          (add-chain updated_board {:color color
                            :stones [point]
                            :liberties liberties}))
        (merge-chains updated_board
                      (concat friendly_chains [{:color color
                                                :stones [point]
                                                :liberties liberties}]))))))

(defn merge-chains
  "merging chains touching a point, heavy processing due to the
  high-maintenance data structure
  at this point we assume it is not a self-capture
  merging to the first"
  [{chains :chains lookup :lookup :as board}
   friendly_chains]
  (let [chain_indices (map (partial index chains) (butlast friendly_chains))
        chain_index (first chain_indices)
        the_chain (first friendly_chains)
        upd_chain (reduce
                   (fn [ch1 ch2]
                     {:color (:color ch1)
                      :stones (into (:stones ch1) (:stones ch2))
                      :liberties (union (:liberties ch1) (:liberties ch2))})
                   the_chain
                   (rest friendly_chains))]
    (println chain_indices)
    (-> board
        (update-in [:chains  chain_index]
                   (constantly upd_chain))
        (update :chains
                (fn [chains] (vec-rm-all chains (rest chain_indices))))
        (update :lookup
                (fn [m] (into m (map (fn [pt] [pt upd_chain]) (:stones upd_chain))))))))

(def single-stone-to-append
  (-> (empty-board 1 2)
      (put-stone [1 1] :b)))
(def two-stones-to-be-merged
  (-> (empty-board 3 3)
      (put-stone [1 2] :b)
      (put-stone [3 2] :b)))

(def atari
  (-> (empty-board 2 2)
      (put-stone [1 1] :w)
      (put-stone [1 2] :b)))

(def ponnuki
  (reduce (fn [board point]
            (put-stone board point :b))
          (empty-board 19 19)
          [[1 2] [2 1] [3 2] [2 3]]))

(defn board-string
  [{width :width height :height chains :chains lookup :lookup :as board}]
  (apply str (apply concat
                   (for [c (range 1 (inc width))]
                     (concat
                      (for [r (range 1 (inc height))]
                        (symbols (:color (lookup [c r]))))
                      '(\newline))))))
