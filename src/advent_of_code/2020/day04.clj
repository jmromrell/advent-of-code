(ns advent-of-code.2020.day04
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as s]))

(defn parse-record [s]
  (apply hash-map (s/split s #"[: \r?\n]+")))

(defn read-part1-input []
  (->> (io/resource "2020/day04-part1-input.txt")
       slurp
       (#(s/split % #"(\r?\n){2,}"))
       (map parse-record)))

(defn year-validation-fn [min-year max-year s]
  (or (and (re-matches #"[0-9]{4}" s)
           (<= min-year (Integer/parseInt s) max-year))))

(def field-validation-fns {"byr" (partial year-validation-fn 1920 2002)
                           "iyr" (partial year-validation-fn 2010 2020)
                           "eyr" (partial year-validation-fn 2020 2030)
                           "hgt" (fn [s]
                                   (when-let [[_ n-str unit] (re-matches #"([0-9]+)(cm|in)" s)]
                                     (case unit
                                       "cm" (<= 150 (Integer/parseInt n-str) 193)
                                       "in" (<= 59 (Integer/parseInt n-str) 76)
                                       false)))
                           "hcl" (partial re-matches #"#[0-9a-f]{6}")
                           "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
                           "pid" (partial re-matches #"[0-9]{9}")})

(defn part1 []
  (->> (read-part1-input)
       (filter (comp #(set/superset? % (keys field-validation-fns))
                     set
                     keys))
       count))

(defn part2 []
  (->> (read-part1-input)
       (filter #(every? (fn [[k validation-fn]]
                          (when-let [v (% k)]
                            (validation-fn v)))
                        field-validation-fns))
       count))