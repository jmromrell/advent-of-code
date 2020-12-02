(ns advent-of-code.2020.day01
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combinatorics]
            [clojure.string :as s]))

(defn read-part1-input []
  (->> (io/resource "2020/day01-part1-input.txt")
       slurp
       (#(s/split % #"\r\n"))
       (map #(Long/parseLong %))))

(defn part1 []
  (->> (read-part1-input)
       (#(combinatorics/combinations % 2))
       (filter #(= 2020 (apply + %)))
       first
       (apply *)))

(defn part2 []
  (->> (read-part1-input)
       (#(combinatorics/combinations % 3))
       (filter #(= 2020 (apply + %)))
       first
       (apply *)))