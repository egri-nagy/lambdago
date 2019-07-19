(ns lgo.board
  "Functions for representing board state and its evolution."
  (:require [lgo.util :refer [vec-rm]]))

;; The board position is stored as a vector of chains, in the order of creation.
;; A chain is represented by its oldest stone.
;; When connecting the newer chain is merged to the older one.


(def symbols {:b \x :w \o nil \.})

;; a chain, the stones order is not guaranteed since connecting could happen
{:player :b, :stones [[3 4] [4 4]]}
