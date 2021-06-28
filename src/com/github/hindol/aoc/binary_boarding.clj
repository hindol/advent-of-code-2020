(ns com.github.hindol.aoc.binary-boarding
  (:require [clojure.java.io :as io]))

(def input-file
  "resources/binary-boarding.txt")

(def decoder
  {\F 0
   \B 1
   \L 0
   \R 1})

(defn binary
  [decoder s]
  (reduce +
          (map-indexed (fn [i v]
                         (if (= 1 (decoder v))
                           (int
                            (Math/pow (* 2 (decoder v)) i))
                           0))
                       (reverse s))))

(def seat-ids
  (into (sorted-set)
        (map #(binary decoder %) (line-seq (io/reader input-file)))))

(comment
  (last seat-ids)
  (filter (fn [[x y]]
            (< 1 (- y x)))
          (partition 2 1 seat-ids)))
