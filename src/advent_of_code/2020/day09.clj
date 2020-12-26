(ns advent-of-code.2020.day09
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn read-part1-input []
  (->> (io/resource "2020/day09-part1-input.txt")
       slurp
       s/split-lines
       (map #(Long/parseLong %))))

(defn contains-distinct-sum? [nums sum]
  (let [nums-set (set nums)]
    (some #(let [target (- sum %)]
             (and (not= target %)
                  (nums-set target)))
          nums)))

(defn get-first-invalid-number [nums]
  (->> nums
       (partition 26 1)
       (some #(let [n (last %)
                    preamble (butlast %)]
                (when-not (contains-distinct-sum? preamble n)
                  n)))))

(defn part1 []
  (get-first-invalid-number (read-part1-input)))

(defn one-pass-stats [coll]
  (when-not (seq coll)
    (throw (IllegalArgumentException. "Cannot collect stats on an empty sequence.")))
  (reduce (fn [{:keys [smallest largest]} n]
            {:smallest (min smallest n)
             :largest (max largest n)})
          {:smallest (first coll)
           :largest (first coll)}
          (rest coll)))

(defn subsequence->encryption-weakness [subseq]
  (let [{:keys [smallest largest]} (one-pass-stats subseq)]
    (+ smallest largest)))

(defn part2 []
  (let [input-nums (read-part1-input)
        target-sum (get-first-invalid-number input-nums)]
    ;Test all subsequences i->j by beginning with all valid `i`s, converting each i to a subseq from i->end, then
    ;iteratively add values and test each partial sum i->i+1, i->i+2, etc. without needing to repeat shared addition
    ;since addition is associative: 1 + 2 + 3 = (1 + 2) + 3
    (->> (range (dec (count input-nums))) ;valid `i`s
         (map #(drop % input-nums)) ;for each i, get subsequence i->end
         (some (fn [subseq]
                 (loop [partial-sum (first subseq) [next & remaining] (rest subseq)]
                   ;exit condition: have tested all ranges of i->j for the current i such that i < j < end
                   ;exit condition: since all numbers are positive, short circuit if sum exceeds target
                   (when (and next (< partial-sum target-sum))
                     (let [next-sum (+ partial-sum next)]
                       (if (= target-sum next-sum)
                         (take (- (count subseq) (count remaining)) subseq) ;return subseq with desired sum, if it exists
                         (recur (+ partial-sum next) remaining)))))))
         subsequence->encryption-weakness)))