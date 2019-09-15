(ns lgo.board
  "Functions for representing board state and its evolution.
  The board position is stored as a vector of chains, in the order of creation.
  A chain is represented by its oldest stone.
   When connecting the newer chain is merged to the older one."
  (:require
   [lgo.grid :refer [neighbours envelope]]
   [lgo.util :refer [vec-rm-all vec-rm]]
   [kigen.position :refer [index position]]
   [clojure.set :refer [union difference]]
   [clojure.string :as string]))

;;for switching between the colors
(def opposite {:b :w, :w :b})

(declare empty-board ;; data structure for an empty board
         put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board
         merge-chains ;;merging friendly chains
         board-string) ;; traditional ASCII rendering of the board

(defn chain_eq_by_stones
  "Deciding equality of chains by the contained stones only. During putting a
  stone the liberties might be out of sync for instances of the same chain.
  This fixes an error, but may not be the best way to do it."
  [chain1 chain2]
  (= (:stones chain1) (:stones chain2)))

(defn empty-board
  "Creates an empty board with the given dimensions."
  [width height]
  {:width width
   :height height
   :chains [] ;; ordered by the age of the chains
   :lookup {}}) ;; points to chains

(defn single-stone-chain
  "Creates a single stone chain.
  A chain is a hash-map , the stones' order is not guaranteed since merging
  will happen."
  [color point liberties]
  {:color color
   :stones [point]
   :liberties liberties})

(defn register-chain
  "updating the lookup table of the board by registering a given chain"
  [board chain]
  (update board :lookup
          (fn [m] (into m (map (fn [pt] [pt chain])
                               (:stones chain))))))

(defn register-chain-by-index
  "Calls register-chain. It is useful when we have the index of the chain to be
  registered but not the chain itself."
  [board n]
  (register-chain board ((:chains board) n)))

(defn recompute-liberties
  "Recomputes liberties of a chain."
  [{width :width height :height chains :chains lookup :lookup :as board} chain]
  (let [e (envelope (:stones chain) width height)
        chain_index (position (partial chain_eq_by_stones chain) chains)]
    (-> board
     (update-in [:chains  chain_index :liberties]
                (fn [l] (union l (set (remove lookup e)))))
     (register-chain-by-index chain_index)))) ;; why do we have to do this?

(defn recompute-liberties-by-point
  "Recomputes liberties of a chain specified by one of its points."
  [board point]
  (recompute-liberties board ((:lookup board) point)))

(defn update-liberties
  "Recomputes liberties for the set of chains given."
  [board chains]
  (reduce recompute-liberties board chains))

(defn add-chain
  "Adding a new chain to a board."
  [board chain]
  (-> board
      ;;adding it to the list of chains
      (update :chains (fn [chains] (conj chains chain)))
      ;;updating the reverse lookup
      (register-chain chain)))

(defn capture-chain
  [{lookup :lookup width :width height :height :as board}
   {stones :stones color :color :as chain}]
  (let [opp (opposite color)
        affected_chains (filter #(= opp (:color %))
                                (distinct
                                 (map lookup
                                      (envelope stones width height))))]
    (-> board
        (update :chains
                (fn [chains] (vec-rm  chains
                                      (index chains chain))))
        (update :lookup
                (fn [m]
                  (apply dissoc m (:stones chain))))
        (update-liberties affected_chains))))

(defn capture-chains
  [board ochains]
  (reduce capture-chain board ochains))

(defn dec-liberties
  [{chains :chains :as board} ochains point]
  (reduce (fn [brd chn]
            (let [chn_index (index chains chn)]
              (-> brd
                  (update-in [:chains chn_index :liberties]
                             #(difference % #{point}))
                  (register-chain-by-index  chn_index))))
          board
          ochains))

(defn put-stone
  "Places a single stone  on the board, updating the chain list.
  For now this is used only for chain analysis, will not check legality of the
  move fully.
  The following things can happen to adjacent points:
  1. if it's empty, it becomes a liberty
  2. when occupied by enemy stones,
    a. its chain captured if the point is its last liberty
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
    (let [adjpts (neighbours point width height) ;;adjacent points, neighbours
          ;; adjacent chains, no duplicates, nils removed
          adj_chains (filter identity (distinct (map lookup adjpts)))
          grouped_chains (group-by :color adj_chains)
          friendly_chains (grouped_chains color)
          opponent_chains (grouped_chains (opposite color))
          to_be_captured (set (filter #(= 1 (count (:liberties %)))
                                      opponent_chains))
          to_be_deced (remove (set to_be_captured) opponent_chains)
          liberties (set (filter #(or (nil? (lookup %))
                                      (to_be_captured (lookup %)))
                                 adjpts))
          nchain (single-stone-chain  color point liberties)
          updated_board (-> board
                            (capture-chains to_be_captured)
                            (dec-liberties to_be_deced point)
                            (add-chain nchain)
                            (merge-chains friendly_chains nchain)
                            (recompute-liberties-by-point point))]
      (if (empty? (:liberties ((:lookup updated_board) point)))
        (do
          (println "self-capture")
          board)
        updated_board))))

(defn merge-chains
  "merging chains to the first one, heavy processing due to the
  high-maintenance data structure
  at this point we assume it is not a self-capture
  merging to the first"
  [{chains :chains lookup :lookup :as board}
   to_be_merged
   connector]
  (if (empty? to_be_merged) ;;nothing to merge
    board
    (let [cs (concat to_be_merged [connector])
          chain_indices (map (fn [c] (position (partial chain_eq_by_stones c) chains)) cs)
          chain_index (first chain_indices)
          the_chain (first cs)
          upd_chain (reduce
                     (fn [ch1 ch2]
                       {:color (:color ch1)
                        :stones (into (:stones ch1) (:stones ch2))
                        :liberties (union (:liberties ch1) (:liberties ch2))})

                     the_chain
                     (rest cs))
          upd2 (update upd_chain :liberties
                       (fn [l] (difference l (set (:stones connector)))))]
      (when (nil? chain_index)
        (println "chain index for merging" chain_index "\n chains in board" chains "\n chains we got fopr merging" cs))
      (-> board
          (update-in [:chains  chain_index]
                     (constantly upd2))
          (update :chains
                  (fn [chains] (vec-rm-all chains (rest chain_indices))))
          (recompute-liberties upd2)
          (register-chain upd2)))))

;; (def ponnuki
;;   (reduce (fn [board point]
;;             (put-stone board point :b))
;;           (empty-board 19 19)
;;           [[1 2] [2 1] [3 2] [2 3]]))

;;for the ASCII rendering of a board
(def symbols {:b \X :w \O nil \.})

(defn board-string
  "The string representation of a board position.
  This relies only on the lookup function of the data structure."
  [{width :width height :height lookup :lookup}]
  (string/join
   (apply concat
          (for [r (range 1 (inc height))]
            (concat
             (for [c (range 1 (inc width))]
               (symbols (:color (lookup [c r]))))
             '(\newline))))))
