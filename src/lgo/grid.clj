(ns lgo.grid
  "Functions for dealing with a rectangular grid.
  Representation: a point is a pair (vector) of integers.
  [column row]")

(declare neighbours
         inside-points
         boundary-points
         envelope)

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

(defn inside-points
  "Returns the stones that are 'inside' in the given group of stones.
  Inside stones are defined by the property of having all of its neighbours in
  the group.
  This is meant for chains, however definition makes sense for general groups.
  Color is not considered here."
  [stones width height]
  (let [S (set stones)]
    (filter (fn [point]
              (every? S (neighbours point width height)))
            S)))

(defn boundary-points
  "Returns the stones that form the 'boundary' of the group.
  Boundary is defined by the set minus the inside (see inside-points).
  This is meant for chains, however definition makes sense for general groups."
  [stones width height]
  (remove (set (inside-points stones width height))
          stones))

(defn envelope
  "Returns the points that are needed to surround the group of stones.
  Method: We compute all neighbours of the boundary, and remove the set
  of original stones from that."
  [stones width height]
  (let [boundary (boundary-points stones width height)
        fullneighbours (reduce (fn [r pt]
                                 (into r (neighbours pt width height )))
                               #{}
                               boundary)]
    (remove (set stones) fullneighbours)))

(defn points
  "Returns all the points of the board of the given dimensions."
  [width height]
  (for [row (range 1 (inc height))
        col (range 1 (inc width))]
    [col row]))
