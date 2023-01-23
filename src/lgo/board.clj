(ns lgo.board
  "Functions for representing a board state and its evolution, by adding stones,
  possibly capturing, and merging the existing chains accordingly.
  The board position is stored as a vector of chains, in the order of creation.
  A chain has a color and a set of stones.
  When connecting friendly chains newer chains are merged to the oldest one.
  Liberties for chains are stored separately, as the set of liberties may change
  even if the chain remains the same.
  For quick access we also have a lookup table from points to chains.
   
  A point is simply a vector, a pair of positive integers in the order of column, row.

  The evolution of the board is traced by creating newer versions of the
  immutable data structure representing the board. Several of the functions
  below produce a changed version of the board. These are 'updating' functions
  in this sense."
  (:require
   [lgo.grid :refer [neighbours points]]
   [lgo.chain :refer [envelope]]
   [lgo.util :refer [vec-rm-all vec-rm index]]
   [clojure.string :as string]))

;;for switching between the colors
(def opposite {:b :w, :w :b})

(declare empty-board ;; data structure for an empty board
         put-stone ;; this puts a stone on a board position
         self-capture? ;;is a move self-capture?
         merge-chains ;;merging friendly chains
         board-string) ;; traditional ASCII rendering of the board

;; BOARD STATE EVOLUTION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions culminate in the put-stone function.

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
  "Updating the lookup table of the board by registering a given chain.
  Each stone in the chain is mapped to the chain itself in the lookup table."
  [board chain]
  (update board :lookup
          (fn [m] (into m (map (fn [pt] [pt chain])
                               (:stones chain))))))

(defn compute-liberties
  "Computes liberties of a chain. This is a fresh calculation (not incremental).
  The algorithm works by removing the occupied points from its envelope.
  Note that these occupied points must be enemy stones, otherwise they would
  belong to the chain."
  [{width :width height :height lookup :lookup} ;; board
   chain]
  (let [e (envelope (:stones chain) width height)]
    (set (remove lookup e)))) ;using truthy-falsey

(defn update-liberties
  "Updates a board by recomputing liberties of an existing chain.
  Calling compute-liberies, thus it is a fresh recompute.
  Updating refers to the lookup table of the board."
  [board chain]
  (update-in board [:liberties  chain]
             (constantly (compute-liberties board chain))))

(defn update-liberties-by-point
  "Updates the liberties of a chain specified by one of its points.
  Just to automate lookup in threading macros."
  [board point]
  (update-liberties board ((:lookup board) point)))

(defn add-chain
  "Adding a new chain to a board. This involves:
  1. adding a chain at the end of the chains vector
  2. registering it in the lookup
  Associating the set of liberties with the chain should be done later." ;TODO why not computing liberties?
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
  "Capturing a chain involves
  1. removing the chain from the vector of chains
  2. updating the liberties of neighbouring chains of opposite color.
  Friendly chains cannot be affected."
  [{lookup :lookup width :width height :height :as board}
   {stones :stones color :color :as chain}]
  (let [opp (opposite color)
        affected_chains (filter #(= opp (:color %))
                                (distinct
                                 (map lookup
                                      (envelope stones width height))))
        bulk-update-liberties (fn [board chains] ;for the threading macro
                                (reduce update-liberties board chains))]
    (-> board
        (remove-chain chain)
        (bulk-update-liberties affected_chains))))

(defn capture-chains
  "Just capturing several chains in one go."
  [board chains]
  (reduce capture-chain board chains))

(defn remove-liberty
  "Removes a single point from the liberties of all given chains."
  [board chains point]
  (reduce (fn [brd chn]
            (update-in brd [:liberties chn] #(disj % point)))
          board
          chains))

(defn merge-chains
  "Merging chains to the first one and updating the board through a new
  connecting single-stone chain, the newly placed stone."
  [{chains :chains :as board}
   friendly_chains
   connector] ; the newly created single-stone chain
  (if (empty? friendly_chains)
    (-> board
        (add-chain connector) ;;nothing to merge, just add the connector chain
        (update-liberties connector))
    (let [chain_indices (map (partial index chains) friendly_chains)
          upd_chain (reduce
                     (fn [ch1 ch2]
                       {:color (:color ch1)
                        :stones (into (:stones ch1) (:stones ch2))})
                     (first friendly_chains) ; merge into the oldest chain
                     (concat (rest friendly_chains) [connector]))]
      (-> board
          ;; updating the oldest chain
          (update-in [:chains  (first chain_indices)]
                     (constantly upd_chain))
          ;;removing all the merged ones
          (update :chains
                  (fn [chains] (vec-rm-all chains (rest chain_indices))))
          (register-chain upd_chain) ;; the merged stones have wrong lookup values
          (update-liberties upd_chain)))))

(defn put-stone
  "Places a single stone  on the board, updating the chain list.
  The following things can happen to adjacent points:
  1. if an adjacent point is empty, then it becomes a liberty of the new chain
  2. when occupied by enemy stones,
    a. its chain captured if the point is its last liberty
    b. its chain needs to be updated by removing the point from its liberties
  3. when occupied by a friendly stone, chains
    a. get merged
    b. possibly captured (self-capture)."
  [{width :width height :height lookup :lookup liberties :liberties :as board}
   color
   point]
  (when-not (lookup point) ; if stone is on board, return nil
    (let [adjpts (neighbours point width height) ;;adjacent points, neighbours
          ;; adjacent chains, no duplicates, nils removed
          adj_chains (remove nil? (distinct (map lookup adjpts)))
          grouped_chains (group-by :color adj_chains)
          friendly_chains (grouped_chains color)
          opponent_chains (grouped_chains (opposite color))
          ;;opponent chains with a single liberty (must be this point) captured
          captured (set (filter #(= 1 (count (liberties %)))
                                opponent_chains))
          affected (remove captured opponent_chains)
          updated_board (-> board
                            (capture-chains captured)
                            (remove-liberty affected point)
                            (merge-chains friendly_chains
                                          (single-stone-chain color point)))]
      ;;if the new has no liberties, then it's a self-capture
      (if (empty? ((:liberties updated_board) ((:lookup updated_board) point)))
        (remove-chain updated_board ((:lookup updated_board) point))
        updated_board))))

;; INFORMATION ABOUT THE BOARD ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These functions query the properties of the board position and hypothetical
;; moves.
(defn empty-board?
  [board]
  (empty? (:chains board)))

(defn self-capture?
  "Returns true if putting stone at the point would be a self-capture."
  [{width :width height :height lookup :lookup liberties :liberties}
   color
   point]
  (let [chains (distinct (map lookup (neighbours point width height)))]
    (when (not-any? nil? chains) ; there is no liberty for stone
      (let [;merge the friendly chains
            merged (reduce into
                           #{point}
                           (map :stones
                                (filter #(= color (:color %))
                                        chains)))
            env (envelope merged width height)
            ochs (distinct (map lookup env))]
        (when (not-any? nil? ochs) ;; no liberty for merged chain
          ;; only enemy chains now, and none of them can be captured by stone
          (not-any? #(= #{point} (liberties %)) ochs))))))

(defn eye-fill?
  "Returns true if putting stone there is filling up an eye.
  This happens when all neighbours have a friendly chain (possible same)."
  [{width :width height :height lookup :lookup}
   color
   point]
  (let [ngbs (neighbours point width height)]
    (every?
     (fn [chain] (and (not (nil? chain))
                   (= color (:color chain))))
     (map lookup ngbs))))

(defn empty-points
  "Returns the empty points of a board position, i.e. all the grid points
  with no stone on them."
  [{lookup :lookup width :width height :height}]
  (filter (comp nil? lookup)
          (points width height)))

;; VISUALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for the ASCII rendering of a board

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

;;
(def colors {\X :b \x :b \O :w \o :w})

(defn build-position
  "Creates the position from the string representation of a board.
  It doesn't check whether the position is legal or not."
  [rows]
  (let [w (count (first rows)) ;;width
        h (count rows) ;;height
        ;;pairing colors (indicating whose turn is it) and points
        paired (map vector
                    (map colors (apply concat rows)) ;converting chars to colors
                    (points w h))
        ;;empty points we don't need to worry
        moves (remove (comp nil? first) paired)]
    (reduce
     (partial apply put-stone)
     (empty-board w h)
     moves)))
