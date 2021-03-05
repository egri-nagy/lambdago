(ns lgo.grid
  "Functions for dealing with points on a rectangular grid.
  Representation: a point is a pair (vector) of integers.
  [column row]")

(declare neighbours
         points)

(defn neighbours
  "Returns the neighbours of a grid point. It considers the size of the board,
  i.e. edges and corners are handled properly.
  Method:
  We generate all neighbours, then filter the valid ones (not the fastest)."
  [[column row] width height]
  (let [points  [[(dec column) row]
                 [(inc column) row]
                 [column (dec row)]
                 [column (inc row)]]]
    (filterv (fn [[c r]] (and (<= 1 c width)
                              (<= 1 r height)))
             points)))

(defn points
  "Returns all the points of the board of the given dimensions."
  [width height]
  (for [row (range 1 (inc height))
        col (range 1 (inc width))]
    [col row]))
