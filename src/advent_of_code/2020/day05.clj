(ns advent-of-code.2020.day05
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn seat->id [seat-str]
  (let [n (-> seat-str
              (clojure.string/replace #"[FL]" "0")
              (clojure.string/replace #"[BR]" "1")
              (Integer/parseInt 2))
        row (bit-shift-right n 3) ;integer value of leading 7 bits with last 3 trimmed
        col (bit-and n 7)] ;last 3 bits (using 7 as bit mask for last 3 bits: 2^3 - 1)
    (+ (* row 8) col)))

(defn read-part1-input []
  (->> (io/resource "2020/day05-part1-input.txt")
       slurp
       s/split-lines
       (map seat->id)))

(defn part1 []
  (apply max (read-part1-input)))

(defn part2 []
  (loop [[id & ids] (sort (read-part1-input))]
    (if ids
      (if (= (first ids) (inc id))
        (recur ids)
        (inc id))
      (throw (IllegalStateException. "Puzzle condition not met.")))))