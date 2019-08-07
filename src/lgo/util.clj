(ns lgo.util
  "Little utility functions.")

(defn vec-rm
  "'Removes' an element (specified by its index) from a vector."
  [^clojure.lang.PersistentVector v i]
  (into (subvec v 0 i) (subvec v (inc i))))

(defn vec-rm-all
  "'Removes' all elements specified by the given indices from a vector."
  [^clojure.lang.PersistentVector v indices]
  (reduce
   (fn [r i]
     (vec-rm r i))
   v
   (reverse (sort indices))))
