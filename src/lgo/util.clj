(ns lgo.util
  "Little utility functions.")

(defn vec-rm
  "'Removes' an element (specified by its index) from a vector."
  [^clojure.lang.PersistentVector v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn vec-rm-all
  "'Removes' all elements specified by the given indices from a vector."
  [^clojure.lang.PersistentVector v indices]
  (reduce vec-rm v (reverse (sort indices))))

(defn index
  "Getting the first index of an element in a vector."
  ;;adapted from kigen
  [^clojure.lang.PersistentVector v elt]
  (first
   (for [[index element] (map vector (range) v)
         :when (= elt element)]
     index))) ;rewrite for performance
