(ns lgo.board
  "Functions for representing board state and its evolution.
  The board position is stored as a vector of chains, in the order of creation.
   A chain is represented by its oldest stone (first?).
   When connecting the newer chain is merged to the older one."
  (:require
   [lgo.grid :refer [neighbours envelope]]
   [lgo.util :refer [vec-rm-all vec-rm]]
   [kigen.position :refer [index]]
   [clojure.set :refer [union difference]]
   [clojure.string :as string]))

;;for switching between the colors
(def opposite {:b :w, :w :b})

(declare empty-board ;; data structure for an empty board
         put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board
         merge-chains ;;merging friendly chains
         board-string) ;; traditional ASCII rendering of the board

(defn empty-board
  "Creates an empty board with the given dimensions."
  [width height]
  {:width width
   :height height
   :chains [] ;; ordered by the age of the chains
   :liberties {} ;; chains to liberties, since the liberties are more volatile
   :lookup {}}) ;; points to chains

(defn single-stone-chain
  "Creates a single stone chain.
  A chain is a hash-map , the stones' order is not guaranteed since merging
  will happen."
  [color point]
  {:color color
   :stones [point]})

(defn register-chain
  "updating the lookup table of the board by registering a given chain"
  [board chain]
  (update board :lookup
          (fn [m] (into m (map (fn [pt] [pt chain])
                               (:stones chain))))))

(defn compute-liberties
  "Computes liberties of a chain. Removing occupied points from its envelope."
  [{width :width height :height lookup :lookup :as board} chain]
  (let [e (envelope (:stones chain) width height)]
    (set (remove lookup e))))


(defn recompute-liberties
  "Recomputes liberties of a chain."
  [board chain]
  (update-in board [:liberties  chain]
             (constantly (compute-liberties board chain))))

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
  [board chain liberties]
  (-> board
      ;;adding it to the list of chains
      (update :chains #(conj % chain))
      ;;registering the liberties separately
      (update :liberties #(conj % [chain liberties]))
      ;;updating the reverse lookup
      (register-chain chain)))

(defn remove-chain
  [board chain]
  (-> board
      (update :chains
              (fn [chains] (vec-rm  chains
                                    (index chains chain))))
      (update :liberties #(dissoc % chain))
      (update :lookup
              (fn [m]
                (apply dissoc m (:stones chain))))))

(defn capture-chain
  [{lookup :lookup width :width height :height :as board}
   {stones :stones color :color :as chain}]
  (let [opp (opposite color)
        affected_chains (filter #(= opp (:color %))
                                (distinct
                                 (map lookup
                                      (envelope stones width height))))]
    (-> board
        (remove-chain chain)
        (update-liberties affected_chains))))

(defn capture-chains
  [board ochains]
  (reduce capture-chain board ochains))

(defn dec-liberties
  [board ochains point]
  (reduce (fn [brd chn]
            (update-in brd [:liberties chn] #(difference % #{point})))
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
  [{width :width height :height chains :chains lookup :lookup liberties :liberties :as board}
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
          adj_chains (remove nil? (distinct (map lookup adjpts)))
          grouped_chains (group-by :color adj_chains)
          friendly_chains (grouped_chains color)
          opponent_chains (grouped_chains (opposite color))
          to_be_captured (set (filter #(= 1 (count (liberties %)))
                                      opponent_chains))
          to_be_deced (remove (set to_be_captured) opponent_chains)
          liberties (set (filter #(or (nil? (lookup %))
                                      (to_be_captured (lookup %)))
                                 adjpts))
          nchain (single-stone-chain  color point)
          updated_board (-> board
                            (capture-chains to_be_captured)
                            (dec-liberties to_be_deced point)
                            (add-chain nchain liberties)
                            (merge-chains (concat friendly_chains [nchain]))
                            (recompute-liberties-by-point point))]
      (if (empty? ((:liberties updated_board) ((:lookup updated_board) point)))
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
   cs]
  (if (= 1 (count cs)) ;;nothing to merge
    board
    (let [chain_indices (map (partial index chains) cs)
          chain_index (first chain_indices)
          the_chain (first cs)
          upd_chain (reduce
                     (fn [ch1 ch2]
                       {:color (:color ch1)
                        :stones (into (:stones ch1) (:stones ch2))})
                     the_chain
                     (rest cs))]

      (-> board
          (update-in [:chains  chain_index]
                     (constantly upd_chain))
          (update :chains
                  (fn [chains] (vec-rm-all chains chain_indices)))
          (add-chain upd_chain (compute-liberties board upd_chain))
          (register-chain upd_chain)))))

(def ponnuki
  (reduce (fn [board point]
            (put-stone board point :b))
          (empty-board 19 19)
          [[1 2] [2 1] [3 2] [2 3]]))

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
