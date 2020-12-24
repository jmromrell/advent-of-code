(ns advent-of-code.2020.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def command->state-transition-fn {"jmp" (fn [state param] (update state :head + param))
                  "acc"                  (fn [state param] (-> state
                                             (update :acc + param)
                                             (update :head inc)))
                  "nop"                  (fn [state _] (update state :head inc))})

(defn parse-state-machine-fn [s]
  (let [[command param-str] (s/split s #" ")
        param (Integer/parseInt param-str)]
    {:command command :param param}))

(defn read-part1-input []
  (->> (io/resource "2020/day08-part1-input.txt")
       slurp
       s/split-lines
       (mapv parse-state-machine-fn)))

(defn run-program [instructions]
  (loop [state {:head 0 :acc 0} visited #{0}]
    (if (>= (:head state) (count instructions)) ;exit condition: ran past end of tape
      {:type (if (= (:head state) (count instructions))
               :success
               :error-out-of-bounds)
       :final-state state}
      (let [{:keys [command param]} (nth instructions (:head state))
            next-state ((command->state-transition-fn command) state param)]
        (if (visited (:head next-state)) ;exit condition: infinite loop detected
          {:type :error-infinite-loop
           :final-state state}
          (recur next-state (conj visited (:head next-state))))))))

(defn part1 []
  (->> (read-part1-input)
       run-program
       :final-state
       :acc))

;There are more efficient ways to do this. Brute forcing is good enough for now. Might revisit later.
(defn part2 []
  (let [instructions (read-part1-input)]
    (->> (range (count instructions))
         (filter #(#{"jmp" "nop"} (get-in instructions [% :command])))
         (map #(update-in instructions [% :command] {"jmp" "nop", "nop" "jmp"})) ;flip command
         (map run-program)
         (some #(when (= :success (:type %)) %))
         :final-state
         :acc)))