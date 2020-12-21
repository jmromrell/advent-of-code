(ns advent-of-code.2020.day03
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-part1-input []
  (->> (io/resource "2020/day03-part1-input.txt")
       slurp
       s/split-lines))

(defn trees-in-direction [tree-grid dir-vector]
  (let [height (count tree-grid)
        width (count (first tree-grid))]
    (->> (iterate (partial mapv + dir-vector) [0 0])
         (take-while (fn [[x y]] (< y height)))
         (map (fn [[x y]] (get-in tree-grid [y (mod x width)])))
         (filter #{\#})
         count)))

(defn part1 []
  (let [input-lines (get-part1-input)]
    (trees-in-direction input-lines [3 1])))

(defn part2 []
  (let [input-lines (get-part1-input)]
    (->> #{[1 1] [3 1] [5 1] [7 1] [1 2]}
         (map (partial trees-in-direction input-lines))
         (apply *))))