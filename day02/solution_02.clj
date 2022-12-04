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

(defn- get-score2
  [line]
  ;; A Rock, B Paper, C Scissors
  ;; X lose, Y draw, Z win
  ({"A X" (+ 0 3) ; lose, scissors
    "A Y" (+ 3 1) ; draw, rock
    "A Z" (+ 6 2) ; win, paper
    "B X" (+ 0 1) ; lose, rock
    "B Y" (+ 3 2) ; draw, paper
    "B Z" (+ 6 3) ; win, scissors
    "C X" (+ 0 2) ; lose, paper
    "C Y" (+ 3 3) ; draw, scissors
    "C Z" (+ 6 1) ; win, rock
    }line))

(defn part-2
  "Day 02 part 2"
  [input]
  (->> input
       str/split-lines
       (map get-score2)
       (reduce +)))

(comment
  (def test-input "A Y
B X
C Z")

  (def output (part-1 test-input))
  (= output 15)

  (def output2 (part-2 test-input))
  (= output2 12))
