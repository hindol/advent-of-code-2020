(ns com.github.hindol.aoc.toboggan-trajectory
  (:require [clojure.java.io :as io]
            [com.github.hindol.aoc.utils :refer [count-if]]))

(def area-map
  (vec (line-seq (io/reader "resources/toboggan-trajectory.txt"))))

(defn at
  [area x y]
  (let [width (-> area (nth 0) count)]
    (get-in area [x (rem y width)])))

(defn locations
  [area next-fn]
  (loop [items (transient [])
         [x y] [0 0]]
    (if-let [item (at area x y)]
      (do
        (conj! items item)
        (recur items (next-fn [x y])))
      (persistent! items))))

(defn count-trees
  [area next-fn]
  (count-if #{\#} (locations area next-fn)))

(reduce *
        [(count-trees area-map (fn [[x y]]
                                 [(inc x) (+ y 3)]))
         (count-trees area-map (fn [[x y]]
                                 [(inc x) (inc y)]))
         (count-trees area-map (fn [[x y]]
                                 [(inc x) (+ y 5)]))
         (count-trees area-map (fn [[x y]]
                                 [(inc x) (+ y 7)]))
         (count-trees area-map (fn [[x y]]
                                 [(+ x 2) (inc y)]))])
