(ns lgo.board
  "Functions for representing board state and its evolution."
  (:require [lgo.util :refer [vec-rm]]))

;; The board position is stored as a vector of chains, in the order of creation.
;; A chain is represented by its oldest stone.
;; When connecting the newer chain is merged to the older one.


(def symbols {:b \X :w \O nil \bg
              .})

;; a chain is a hash-map , the stones order is not guaranteed since connecting could happen
;; internally we use numbers for coordinates
{:player :b, :stones [[3 4] [4 4]]}

;;board position is a vector of chains + zobrist hash + whose move is that

(declare put-stone ;; this puts a stone on a board position
         legal-move? ;;decides whether the is legal or not on a given board position, ko?
         )
