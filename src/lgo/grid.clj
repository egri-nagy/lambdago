(ns lgo.grid
  "Functions for dealing with points on a rectangular grid.
  Representation: a point is a pair (vector) of integers.
  [column row]")

(declare neighbours
         points)

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
