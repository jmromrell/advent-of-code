(ns advent-of-code.2020.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn normalize-input-line [s]
  (let [[_ n1 n2 c s] (re-find #"([0-9]+)-([0-9]+) ([^:]): (.+)" s)]
    {:n1 (Long/parseLong n1) :n2 (Long/parseLong n2) :character (first c) :password s}))

(defn get-part1-input []
  (->> (io/resource "2020/day02-part1-input.txt")
       slurp
       (#(s/split % #"\r\n"))
       (map normalize-input-line)))

(defn part1 []
  (->> (get-part1-input)
       (filter (fn [{:keys [n1 n2 character password]}]
                 (<= n1 ((frequencies password) character 0) n2)))
       count))

(defn part2 []
  (->> (get-part1-input)
       (filter (fn [{:keys [n1 n2 character password]}]
                 (->> [n1 n2]
                      (map #(nth password (dec %) nil))
                      (filter #{character})
                      count
                      (= 1))))
       count))