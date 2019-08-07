(ns lgo.grid
  "Functions for dealing with a rectangular grid.")

(defn neighbours
  "Neighbours of a grid point considering the size of the board,
  i.e. edges and corners are handled.
  We generate all, then filter the valid ones (not the fastest)."
  [[column row :as point] width height]
  (let [points  [[(dec column) row]
                 [(inc column) row]
                 [column (dec row)]
                 [column (inc row)]]]
    (filterv (fn [[c r]] (and (<= 1 c width)
                              (<= 1 r height)))
             points)))

(defn inside-points
  "Returns the stones that are 'inside' in the given group of stones.
  This is meant for chains, however definition makes sense for general groups."
  [stones width height]
  (let [S (set stones)]
    (filter (fn [point] (every? S (neighbours point width height)))
            S)))

(defn boundary-points
  "Returns the stones that are 'inside' in the given group of stones.
  This is meant for chains, however definition makes sense for general groups."
  [stones width height]
  (remove (set (inside-points stones width height))
          stones))

(defn envelope
  [stones width height]
  (let [boundary (boundary-points stones width height)
        inside (set (inside-points stones width height))
        fullneighbours (reduce (fn [r pt]
                                 (into r (neighbours pt width height )))
                               #{}
                               boundary)]
    (remove inside fullneighbours)))
