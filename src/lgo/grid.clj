(ns lgo.grid
  "Functions for dealing with points on a rectangular grid.
  Representation: a point is a pair (vector) of integers.
  [column row]
   Column and row values start with 1.")

(declare neighbours
         points
         envelope)

(defn neighbours
  "Returns the neighbours of a grid point. It considers the size of the board,
  i.e. edges and corners are handled properly."
  [[column row] width height]
  (filterv identity ;removes nils
           [(when-not (= 1 column) [(dec column) row]) ;gives nil otherwise
            (when-not (= 1 row) [column (dec row)])
            (when-not (= width column) [(inc column) row])
            (when-not (= height row) [column (inc row)])]))

(defn points
  "Returns all the points of the board of the given dimensions (in no
   particular order)."
  [width height]
  (for [row (range 1 (inc height))
        col (range 1 (inc width))]
    [col row]))

(defn envelope
  "Returns the points that need to be occupied by enemy stones to surround
  the group of stones. Note: this is not about a given board postion, just
  a general computation - colors of stones, and thus enemy stones
  are not checked.
  Method: We compute all neighbours of the set of stones, and remove the set
  of original stones from that."
  [stones width height]
  (let [allneighbours (reduce (fn [r pt]
                                (into r (neighbours pt width height)))
                              #{}
                              stones)]
    (remove (set stones) allneighbours)))

