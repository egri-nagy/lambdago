(ns lgo.board
  "Functions for representing a board state and its evolution by adding stones,
  and by possibly capturing and merging the existing chains accordingly.
  The board position is stored as a vector of chains, in the order of creation.
  A chain has a color and a set of stones.
  When connecting friendly chains newer chains are merged into the oldest one.
  Liberties for chains are stored separately, as the set of liberties may change
  even if the chain remains the same.
  For quick access we also have a lookup table from points to chains.
   
  A point is simply a vector, a pair of positive integers in the order of column, row.

  The evolution of the board is traced by creating newer versions of the
  immutable data structure representing the board. Several of the functions
  below produce a changed version of the board. These are called 'updating'
  functions in this sense only.
   
  The basic unit is the chain. A single stone is just a one-stone chain.
  Thus anything that can happen to the board happens through add-chain and
   remove-chain."
  (:require
   [lgo.grid :refer [neighbours points envelope]]
   [clojure.set :refer [difference]]
   [clojure.string :as string]))

;;for switching between the colors
(def opponent {:b :w, :w :b})

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
   :chains #{} ;; set of chains
   :liberties {} ;; chains to sets of liberties
   :lookup {} ;;points to chains
   :empties (set (points width height))}) ;; set of empty points

(defn single-stone-chain
  "Creates a single stone chain given a point and color of the stone."
  [color point]
  {:color color
   :stones [point]})

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

(defn bulk-update-liberties
  [board chains]
  (reduce update-liberties board chains))

(defn affected-chains
  "Finds all the chains that can be affected by the given chain's
   addition or removal. Calculates the envelope and finds the chains on
   those points."
  [board chain]
  (remove nil? (distinct
                (map (:lookup board)
                     (envelope (:stones chain) (:width board) (:height board))))))


(defn add-chain
  "Adding a new chain to a board. This involves:
  1. adding a chain to the set of chains
  2. registering it in the lookup
  3. associating the set of liberties with the chain
  4. updating liberties of affected chains 
  5. removing the chain stones from the empty empties."
  [board chain]
  (let [affected (affected-chains board chain)]
    (-> board
        ;adding it to the set of chains, m stands for map
        (update :chains (fn [m] (conj m chain)))
        ;updating the reverse lookup
        (update :lookup (fn [m] (into m (map (fn [pt] [pt chain])
                                             (:stones chain)))))
        ;computing liberties for the new chain
        (update-liberties chain)
        ;recompute liberties for affected chains
        (bulk-update-liberties affected)
        ;removing newly occupied points from empty intersections
        (update :empties (fn [s] (difference s (set (:stones chain))))))))

(defn remove-chain
  "The inverse of add-chain, same order of steps, also removing liberties."
  [board chain]
  (let [affected (affected-chains board chain)]
   (-> board
       (update :chains (fn [chains] (disj chains chain)))
       (update :lookup (fn [m] (apply dissoc m (:stones chain))))
       (update :liberties #(dissoc % chain))
       (bulk-update-liberties affected)
       (update :empties (fn [s] (into s (:stones chain)))))))

(defn bulk-remove-chains
  [board chains]
  (reduce (fn [b c] (remove-chain b c))
          board
          chains))

(defn remove-liberty
  "Removes a single point from the liberties of all given chains."
  [board chains point]
  (reduce (fn [brd chn]
            (update-in brd [:liberties chn] #(disj % point)))
          board
          chains))

(defn merge-chains
  "Merging the given existing chains by removing them and adding their
   unified chains.
   Not adding through the dedicated methods, since we can save recomputing
   liberties."
  [{chains :chains liberties :liberties :as board}
   chains_to_be_merged] 
  (let [stones (reduce
                   (fn [result chain]
                     (into result (:stones chain)))
                   #{}
                   chains_to_be_merged)
        merged {:stones stones
                :color (:color (first chains_to_be_merged))}]
    (-> board
        ;;removing all the merged ones
        (update :chains (fn [m] (reduce disj m chains_to_be_merged)))
        ;;add the merged one
        (update :chains (fn [m] (conj m merged)))
        ;;update lookup table
        (update :lookup (fn [m] (into m (map (fn [pt] [pt merged])
                                             (:stones merged))))) ;TODO bring back register-chain
        (update-in [:liberties  merged]
                   (constantly (reduce into (map liberties chains_to_be_merged))))
        (update :liberties (fn [m] (reduce dissoc m chains_to_be_merged))))))

(defn put-stone
  "Places a single stone on the board. The general strategy is to place
   the stone as a new chain. Removed the captured chains if any, and finally
   merge the friendly chains.
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
          opponent_chains (grouped_chains (opponent color))
          ;;opponent chains with a single liberty (must be this point) captured
          captured (set (filter #(= 1 (count (liberties %)))
                                opponent_chains))
          affected (remove captured adj_chains)
          nchain (single-stone-chain color point)
          updated_board (-> board
                            (bulk-remove-chains captured)
                            (remove-liberty affected point)
                            (add-chain nchain)
                            (merge-chains (conj friendly_chains nchain)))
          finished_new_chain ((:lookup updated_board) point)]
      
      ;;if the new has no liberties, then it's a self-capture
      (if (empty? ((:liberties updated_board) finished_new_chain))
        (remove-chain updated_board finished_new_chain) ;technically we allow self-capture
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

(defn empty-points ;TODO store the empty points explicitly, this would simplify the bots
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
