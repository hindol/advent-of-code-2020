(ns com.github.hindol.aoc.report-repair
  (:require [clojure.java.io :as io]))

(def ^:dynamic *input-file*
  "resources/report-repair.txt")

(defn str->int
  [s]
  (Integer/parseInt s))

(defn entries
  [input]
  (let [lines (line-seq (io/reader input))]
    (map str->int lines)))

(defn find-entries-with-sum
  [es sum]
  (let [es (into #{} es)]
    (->> es
         (filter #(contains? es (- sum %))))))

(defn product-of-two
  "Returns the product of the two numbers whose sum equals `sum`."
  [es sum]
  (reduce * (find-entries-with-sum es sum)))

(defn pairwise-sums
  [es]
  (into {}
        (for [x     es
              y     es
              :when (<= x y)]
          [(+ x y) [x y]])))

(defn product-of-three
  "Returns the product of the three numbers whose sum equals `sum`."
  [es sum]
  (let [sums (pairwise-sums es)]
    (reduce *
            (first
             (for [e     es
                   :when (contains? sums (- sum e))]
               (into [e] (get sums (- sum e))))))))

(comment
  (product-of-two (entries *input-file*) 2020)
  (product-of-three (entries *input-file*) 2020))
