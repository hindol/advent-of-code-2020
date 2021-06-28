(ns com.github.hindol.aoc.utils
  (:refer-clojure :exclude [int]))

(defn count-if
  [pred coll]
  (count (filter pred coll)))

(defn int
  [x]
  (if (string? x)
    (when (re-matches #"\d+" x)
      (Integer/parseInt x))
    (clojure.core/int x)))

