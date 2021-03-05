(ns lgo.chain
  "Functions for chains of stones."
  (:require [lgo.grid :refer [neighbours]]))

(declare inside-points
         boundary-points
         envelope)

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

