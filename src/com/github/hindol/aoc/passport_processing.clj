(ns com.github.hindol.aoc.passport-processing
  (:refer-clojure :exclude [int])
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-file
  "resources/passport-processing.txt")

(defn int
  [x]
  (if (string? x)
    (when (re-matches #"\d+" x)
      (Integer/parseInt x))
    (clojure.core/int x)))

(defn count-if
  [pred coll]
  (count (filter pred coll)))

(defn into-map
  [s]
  (let [pairs (str/split s #" ")]
    (into {}
          (map #(str/split % #":") pairs))))

(defn read-passports
  [f]
  (->> f
       io/reader
       line-seq
       (partition-by #{""})
       (remove #{[""]})
       (map #(->> % (str/join " ") into-map))))

(def required-fields
  ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"])

(defn digit?
  [x]
  (<= (int \0) (int x) (int \9)))

(defn year?
  [s]
  (and
   (every? digit? s)
   (= 4 (count s))))

(defn valid-height?
  [s]
  (when-let [[_ height unit] (re-matches #"(\d+)(cm|in)" s)]
    (if (= unit "cm")
      (<= 150 (int height) 193)
      (<= 59 (int height) 76))))

(def field-rules
  {"byr" #(and (year? %)
               (<= 1920 (int %) 2002))
   "iyr" #(and (year? %)
               (<= 2010 (int %) 2020))
   "eyr" #(and (year? %)
               (<= 2020 (int %) 2030))
   "hgt" valid-height?
   "hcl" #(re-matches #"#[0-9a-f]{6}" %)
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" #(re-matches #"\d{9}" %)})

(defn valid-passport?
  ([passport]
   (valid-passport? passport  {}))
  ([passport rules]
   (and (every? passport required-fields)
        (every? identity
                (for [[k pred] rules
                      :when    (contains? passport k)]
                  (pred (get passport k)))))))

(comment
  (count-if valid-passport? (read-passports input-file))
  (count-if #(valid-passport? % field-rules) (read-passports input-file)))
