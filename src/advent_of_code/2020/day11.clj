(ns advent-of-code.2020.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def FLOOR_STATE \.)
(def EMPTY_STATE \L)
(def OCCUPIED_STATE \#)

(def DIRECTION_VECTORS #{[-1 -1] [0 -1] [1 -1]
                         [-1  0]        [1  0]
                         [-1  1] [0  1] [1  1]})

(defn vector-add [& vectors]
  (apply (partial mapv +) vectors))

(defn invert-2d-grid
  "Change from vector-of-rows format to vector-of-columns format to support get-in with conventional [x y] coordinates."
  [grid]
  (apply (partial mapv (fn [& vals] (vec vals)))
         grid))

(defn read-part1-input []
  (->> (io/resource "2020/day11-part1-input.txt")
       slurp
       s/split-lines
       invert-2d-grid))

(defn generate-2d-grid
  "Create a 2D grid in vector-of-vector format using the given function to map coordinates to their values."
  [w h coordinate->value]
  (vec (for [x (range w)]
         (vec (for [y (range h)]
                (coordinate->value [x y]))))))

(defn state-transition-fn [neighbor-fn empty-seat-threshold seat-grid]
  (when-not (and (seq seat-grid) (seq (first seat-grid)))
    (throw (IllegalArgumentException. "Seat grid must be non-nil and have positive width and height.")))
  (let [w (count seat-grid)
        h (count (first seat-grid))]
    (generate-2d-grid w h
      (fn [position-vector]
        (let [position-state (get-in seat-grid position-vector)]
          (if (= position-state FLOOR_STATE)
            FLOOR_STATE
            (let [num-occupied-neighbors (->> position-vector
                                              (neighbor-fn seat-grid)
                                              (filter #{OCCUPIED_STATE})
                                              count)]
              (if (>= num-occupied-neighbors (empty-seat-threshold position-state))
                EMPTY_STATE
                OCCUPIED_STATE))))))))

(defn simulate-until-stable [neighbor-fn empty-seat-threshold seat-grid]
  (loop [current-state seat-grid visited-states #{}]
    (if (visited-states current-state)
      current-state ;escape condition: return first repeated state
      (recur (state-transition-fn neighbor-fn empty-seat-threshold current-state)
             (conj visited-states current-state)))))

(defn get-adjacent-neighbors [seat-grid position-vector]
  (->> DIRECTION_VECTORS
       (map (partial vector-add position-vector)) ;one step in each direction from the current position
       (map #(get-in seat-grid % nil)))) ;lookup value, default to nil if off-grid

(defn part1 []
  (->> (read-part1-input)
       (simulate-until-stable get-adjacent-neighbors {EMPTY_STATE 1, OCCUPIED_STATE 4})
       flatten
       (filter #{OCCUPIED_STATE})
       count))

(defn get-directional-neighbors [seat-grid origin-coordinate]
  (map (fn [dir-vector]
         (->> origin-coordinate
              (iterate (partial vector-add dir-vector)) ;lazy sequence of all steps from origin in given direction
              rest ;do not include origin in sequence
              (map #(get-in seat-grid % nil))
              (filter (complement #{FLOOR_STATE}))
              first)) ;first non-floor in direction, including nil for out of bounds
       DIRECTION_VECTORS))

(defn part2 []
  (->> (read-part1-input)
       (simulate-until-stable get-directional-neighbors {EMPTY_STATE 1, OCCUPIED_STATE 5})
       flatten
       (filter #{OCCUPIED_STATE})
       count))