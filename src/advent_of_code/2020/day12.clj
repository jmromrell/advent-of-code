(ns advent-of-code.2020.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn parse-instruction [s]
  [(first s) (Long/parseLong (subs s 1))])

(defn read-part1-input []
  (->> (io/resource "2020/day12-part1-input.txt")
       slurp
       s/split-lines
       (map parse-instruction)))

(defn degrees->radians [deg] (* deg 1/180 Math/PI))
(defn round-to-integer [n] (long (Math/round ^double n)))

(defn vector-rotate
  "Rotate a given vector clockwise the given number of radians around the origin [0 0]."
  [[x y] rad]
  (let [cos (Math/cos rad)
        sin (Math/sin rad)]
    [(round-to-integer (- (* x cos) (* y sin)))
     (round-to-integer (+ (* x sin) (* y cos)))]))

(defn vector-add
  "Perform vector addition between any number of provided vectors."
  [& vectors]
  (apply (partial mapv +) vectors))

(defn vector-scale
  "Scales a vector by a factor of N, maintaining direction."
  [vector n]
  (mapv (partial * n) vector))

(defn vector-length-manhattan
  "Get the length of the vector using manhattan distance (the sum of the length of the vector in each dimension)."
  [vector]
  (->> vector
       (map #(Math/abs ^long %))
       (apply +)))

(def NORTH [ 0 -1])
(def EAST  [ 1  0])
(def SOUTH [ 0  1])
(def WEST  [-1  0])

(defn move [coordinate dir-vector distance]
  (-> dir-vector
      (vector-scale distance)
      (vector-add coordinate)))

(def PART1_STATE_TRANSITION_FUNCTIONS
  {\N (fn [state n] (update state :position #(move % NORTH n)))
   \E (fn [state n] (update state :position #(move % EAST n)))
   \S (fn [state n] (update state :position #(move % SOUTH n)))
   \W (fn [state n] (update state :position #(move % WEST n)))
   \F (fn [state n] (update state :position #(move % (:direction-vector state) n)))
   \L (fn [state n] (update state :direction-vector #(vector-rotate % (degrees->radians (- n)))))
   \R (fn [state n] (update state :direction-vector #(vector-rotate % (degrees->radians n))))})

(defn part1 []
  (->> (read-part1-input)
       (reduce (fn [state [command n]]
                 (let [transition-fn (PART1_STATE_TRANSITION_FUNCTIONS command)]
                   (transition-fn state n)))
               {:position [0 0] :direction-vector EAST})
       :position
       vector-length-manhattan))

(def PART2_STATE_TRANSITION_FUNCTIONS
  {\N (fn [state n] (update state :waypoint #(move % NORTH n)))
   \E (fn [state n] (update state :waypoint #(move % EAST n)))
   \S (fn [state n] (update state :waypoint #(move % SOUTH n)))
   \W (fn [state n] (update state :waypoint #(move % WEST n)))
   \F (fn [state n] (update state :position #(move % (:waypoint state) n)))
   \L (fn [state n] (update state :waypoint #(vector-rotate % (degrees->radians (- n)))))
   \R (fn [state n] (update state :waypoint #(vector-rotate % (degrees->radians n))))})

(defn part2 []
  (->> (read-part1-input)
       (reduce (fn [state [command n]]
                 (let [transition-fn (PART2_STATE_TRANSITION_FUNCTIONS command)]
                   (transition-fn state n)))
               {:position [0 0] :waypoint [10 -1]})
       :position
       vector-length-manhattan))