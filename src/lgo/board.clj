(ns lgo.board
  "Functions for representing board state and its evolution, by adding stones
  and capturing, or merging chains accordingly.
  The board position is stored as a vector of chains, in the order of creation.
  A chain has a color and a set of stones.
  When connecting friendly chains the newer chain is merged to the older one.
  Liberties for chains are stored separately, as the set of liberties may change
  even if the chain remains the same.
  For quick access we also have a lookup table from points to chains.

  The evolution of the board is traced by creating newer versions of the
  immutable data structure representing the board. Several of the functions
  below produce a changed version of the board. These are 'updating' functions
  in this sense."
  (:require
   [lgo.grid :refer [neighbours envelope]]
   [lgo.util :refer [vec-rm-all vec-rm]]
   [kigen.position :refer [index]]
   [clojure.string :as string]))

;;for switching between the colors
(def opposite {:b :w, :w :b})

(declare empty-board ;; data structure for an empty board
         put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board
         merge-chains ;;merging friendly chains
         board-string) ;; traditional ASCII rendering of the board

(defn empty-board
  "Creates an empty board with the given dimensions. It returns a hash-map with
  an empty vector of chains, and two hash-maps for lookup and liberties."
  [width height]
  {:width width
   :height height
   :chains [] ;; ordered by the age of the chains
   :liberties {} ;; chains to sets of liberties
   :lookup {}}) ;; points to chains

(defn single-stone-chain
  "Creates a single stone chain given a point and color of the stone."
  [color point]
  {:color color
   :stones [point]})

(defn register-chain
  "Updating the lookup table of the board by registering a given chain."
  [board chain]
  (update board :lookup
          (fn [m] (into m (map (fn [pt] [pt chain])
                               (:stones chain))))))

(defn compute-liberties
  "Computes liberties of a chain. This is a fresh calculation (not incremental)
  by removing the points of the chain from its envelope."
  [{width :width height :height lookup :lookup} ;; board
   chain]
  (let [e (envelope (:stones chain) width height)]
    (set (remove lookup e))))

(defn recompute-liberties
  "Updates a board by recomputing liberties of an existing chain."
  [board chain]
  (update-in board [:liberties  chain]
             (constantly (compute-liberties board chain))))

(defn recompute-liberties-by-point
  "Recomputes liberties of a chain specified by one of its points.
  Just to automate lookup in threading macros."
  [board point]
  (recompute-liberties board ((:lookup board) point)))

(defn update-liberties
  "Recomputes liberties for the set of chains given.
  Only for compatibility with threading macro."
  [board chains]
  (reduce recompute-liberties board chains))

(defn add-chain
  "Adding a new chain to a board. This involves:
  1. adding a chain at the end of the chains vector
  2. registering it in the lookup
  Associating the set of liberties with the chain can be done later."
  [board chain]
  (-> board
      ;;adding it to the vector of chains
      (update :chains #(conj % chain))
      ;;updating the reverse lookup
      (register-chain chain)))

(defn remove-chain
  "The opposite of add-chain, same order of steps, also removing liberties."
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
  "Capturing a chain involves updating the liberties of neighbouring
  chains of opposite color."
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
  "Just capturing several chains in one go."
  [board chains]
  (reduce capture-chain board chains))

(defn dec-liberties
  "Decrementing the liberties of some affected chains by removing a point."
  [board chains point]
  (reduce (fn [brd chn]
            (update-in brd [:liberties chn] #(disj % point)))
          board
          chains))

(defn merge-chains
  "Merging chains to the first one and updating the board."
  [{chains :chains :as board}
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
          ;; updating the oldest chain
          (update-in [:chains  chain_index]
                     (constantly upd_chain))
          ;;removing all the merged ones
          (update :chains
                  (fn [chains] (vec-rm-all chains (rest chain_indices))))
          (register-chain upd_chain) ;; the merged stones have wrong lookup
          (recompute-liberties upd_chain)))))

(defn put-stone
  "Places a single stone  on the board, updating the chain list.
  The following things can happen to adjacent points:
  1. if it's empty, it becomes a liberty
  2. when occupied by enemy stones,
    a. its chain captured if the point is its last liberty
    b. needs to be updated by removing the point from its liberties
  3. when occupied by a friendly stone, chains
    a. get merged
    b. possibly captured."
  [{width :width height :height lookup :lookup liberties :liberties :as board}
   color
   point]
  (if (lookup point)
    ;;illegal move, it's on the board already, just return the same state
    board
    ;;otherwise the stone is not on the board yet, we do the full change and
    ;;rollback it's a self-capture
    (let [adjpts (neighbours point width height) ;;adjacent points, neighbours
          ;; adjacent chains, no duplicates, nils removed
          adj_chains (remove nil? (distinct (map lookup adjpts)))
          grouped_chains (group-by :color adj_chains)
          friendly_chains (grouped_chains color)
          opponent_chains (grouped_chains (opposite color))
          to_be_captured (set (filter #(= 1 (count (liberties %)))
                                      opponent_chains))
          to_be_deced (remove to_be_captured opponent_chains)
          nchain (single-stone-chain color point)
          updated_board (-> board
                            (capture-chains to_be_captured)
                            (dec-liberties to_be_deced point)
                            (add-chain nchain) ;;adding then removing?
                            (merge-chains (concat friendly_chains [nchain]))
                            (recompute-liberties-by-point point))]
      (if (empty? ((:liberties updated_board) ((:lookup updated_board) point)))
        board ;;self-capture
        updated_board))))

(defn legal-move?
  "It's legal if we can put it on the board."
  [board color point]
  (= board (put-stone board color point)))

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
