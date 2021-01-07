(ns advent-of-code.2020.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn get-part1-input []
  (->> (io/resource "2020/day10-part1-input.txt")
       slurp
       s/split-lines
       (map #(Integer/parseInt %))))

(defn part1 []
  (->> (get-part1-input)
       sort
       (cons 0) ;start with 0-rated wall adapter
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       frequencies
       ((fn [freqs]
          (let [ones (or (freqs 1) 0)
                threes (inc (or (freqs 3) 0))] ;increment threes count to account for max+3 rated device
            (* ones threes))))))

(defn count-arrangements
  "We can use dynamic programming to reason about all arrangements in linear instead of combinatorial time
   If there is an adapter for value N, the number of valid permutations that end with N is the sum of permutations that
   ended at adapters for the prior three values: N-1, N-2, and N-3. Thus we can maintain a complete count of all valid
   permutations by tracking only the permutation counts for the last three consecutive integers."
  [sorted-seq]
  (if-not (seq sorted-seq)
    (throw (IllegalArgumentException. "Expects non-empty sequence."))
    ;Begin at N=1 and initialize offsets to [1 0 0] to represent all sequences beginning at 0 (N-1)
    (loop [ints (rest (range)) adapters sorted-seq offset-counts (list 1 0 0)]
      (if-not (seq adapters)
        (first offset-counts) ;Escape condition: device requires max+3, only count permutations reaching the max value
        (if (= (first ints) (first adapters))
          (recur (rest ints)
                 (rest adapters)
                 (-> offset-counts
                     butlast ;Combinations ending in N-3 are no longer considered since they will be unable to reach N+1
                     (conj (apply + offset-counts)))) ;Any combinations ending in N-1, N-2, or N-3 can optionally jump to N
          (recur (rest ints)
                 adapters
                 (-> offset-counts
                     butlast ;Combinations ending in N-3 are no longer considered since they will be unable to reach N+1
                     (conj 0)))))))) ;No combinations can jump to N as it is not a valid adapter

(defn part2 []
  (->> (get-part1-input)
       sort
       count-arrangements))