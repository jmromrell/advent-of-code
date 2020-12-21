(ns advent-of-code.2020.day06
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-part1-input []
  (->> (io/resource "2020/day06-part1-input.txt")
       slurp
       (#(s/split % #"\r?\n\r?\n"))
       (map #(map set (s/split-lines %)))))

(defn part1 []
  (->> (read-part1-input)
       (map #(count (apply clojure.set/union %)))
       (reduce +)))

(defn part2 []
  (->> (read-part1-input)
       (map #(count (apply clojure.set/intersection %)))
       (reduce +)))