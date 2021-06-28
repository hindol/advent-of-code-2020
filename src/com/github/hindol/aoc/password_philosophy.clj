(ns com.github.hindol.aoc.password-philosophy
  (:refer-clojure :exclude [int])
  (:require [clojure.java.io :as io]
            [com.github.hindol.aoc.utils :refer [int count-if]]))

(def input-file
  "resources/password-philosophy.txt")

(def line-regex
  #"(\d+)-(\d+) (\w): (.*)")

(defn get-many
  "Returns the values corresponding to `keys` from map `m`. If `m` is
  sequential, `keys` are assumed to be indices."
  [m keys]
  (for [k keys]
    (get m k)))

(defn seqify
  "Returns a sequence if `thing` is not sequential, else the `thing` itself."
  [thing]
  (if (sequential? thing)
    thing
    [thing]))

(defn update-values
  [update-fns m]
  (reduce-kv (fn [m k v]
               (if-let [f (get update-fns k)]
                 (assoc m k (f v))
                 (assoc m k v)))
             {}
             m))

(defn parse-line
  [re line]
  (->> line
       (re-matches re)
       rest))

(def input-items
  (let [lines (-> input-file io/reader line-seq)]
    (->> lines
         (map #(->> %
                    (parse-line line-regex)
                    (zipmap [:x :y :letter :password])
                    (update-values {:x int
                                    :y int
                                    :letter first}))))))

(defn valid-password?
  "Returns true if the password conforms to the password policies. Each policy
  must be a predicate function which can be applied to the password."
  [policies password]
  (let [policies (seqify policies)
        combined-policy (apply every-pred policies)]
    (combined-policy password)))

(defn make-first-policy
  [{:keys [x y letter]}]
  #(<= x (count (filter #{letter} %)) y))

(defn make-second-policy
  [{:keys [x y letter]}]
  #(= 1 (count-if #{letter} (get-many % [(dec x) (dec y)]))))

(comment
  (count-if true?
            (for [item input-items]
              (valid-password? (make-first-policy item) (:password item))))
  (count-if true?
            (for [item input-items]
              (valid-password? (make-second-policy item) (:password item)))))
