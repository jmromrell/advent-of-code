(ns advent-of-code.2020.day07
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn de-pluralize [s]
  (if (.endsWith s "s")
    (subs s 0 (dec (count s)))
    s))

(defn normalize-bag-rule [s]
  (let [[bag-name content-str] (s/split s #"s contain ")]
    {bag-name
     (if (= content-str "no other bags.")
       {}
       (->> (re-seq #"([0-9]+) ([a-zA-Z ]+)" content-str)
            (map (fn [[_ count-str bag-name]]
                   {(de-pluralize bag-name) (Integer/parseInt count-str)}))
            (into {})))}))

(defn read-part1-input []
  (->> (io/resource "2020/day07-part1-input.txt")
       slurp
       s/split-lines
       (map normalize-bag-rule)
       (into {})))

(defn invert-bag-graph [bag-graph]
  (->> bag-graph
       (mapcat (fn [[outer-bag inner-bag-counts]]
                 (for [inner-bag (keys inner-bag-counts)]
                   [inner-bag outer-bag])))
       (reduce (fn [inverted-graph [inner-bag outer-bag]]
                 (update inverted-graph inner-bag #(conj (set %) outer-bag)))
               {})))

(defn part1 []
  (let [bag->parents (invert-bag-graph (read-part1-input))]
    ;walk graph, keeping track of already-visited nodes to avoid cycles, duplicates, and simply improve performance
    (loop [bags-to-check (bag->parents "shiny gold bag")
           visited-bag-set #{}]
      (if (empty? bags-to-check)
        (count visited-bag-set) ;escape condition - we want the count of visitable bags for part1
        (let [current-bag (first bags-to-check)
              new-visited-set (conj visited-bag-set current-bag)
              new-bags-to-check (-> current-bag bag->parents (set/difference new-visited-set))]
          (recur (-> bags-to-check
                     (disj current-bag)
                     (set/union new-bags-to-check))
                 new-visited-set))))))

(defn part2 []
  (let [bag->contents (read-part1-input)
        count-cache-atom (atom {}) ;syntactically awkward to create a locally scoped recursive memoized fn, so do it manually
        get-bag-count (fn get-bag-count [bag]
                              (or (get @count-cache-atom bag)
                                  (let [inner-bag-count (->> bag
                                                             bag->contents
                                                             (map (fn [[inner-bag count]]
                                                                    (* count (get-bag-count inner-bag))))
                                                             (apply +)
                                                             inc)] ;increment to include the outer bag in the count
                                    (swap! count-cache-atom assoc bag inner-bag-count)
                                    inner-bag-count)))]
    (dec (get-bag-count "shiny gold bag")))) ;do not include the outermost shiny gold bag