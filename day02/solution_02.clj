(ns day02.solution-02 
  (:require [clojure.string :as str]))

(defn- get-score
  [line]
  ;; A Rock, B Paper, C Scissors
  ;; X Rock, Y Paper, Z Scissors
  ({"A X" (+ 1 3)
    "A Y" (+ 2 6)
    "A Z" (+ 3 0)
    "B X" (+ 1 0)
    "B Y" (+ 2 3)
    "B Z" (+ 3 6)
    "C X" (+ 1 6)
    "C Y" (+ 2 0)
    "C Z" (+ 3 3)} line))

(defn part-1
  "Day 02 part 1"
  [input]
  (->> input
       str/split-lines
       (map get-score)
       (reduce +)))

(defn part-2
  "Day 02 part 2"
  [input]
  0)

(comment
  (def test-input "A Y
B X
C Z")
   
  (def output (part-1 test-input))
  (= output 15))
