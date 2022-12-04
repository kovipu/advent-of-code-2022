(ns day04.solution-04
  (:require [clojure.string :as str]))

(defn- parse-line [line]
  (let [matches (re-find (re-matcher #"(\d+)-(\d+),(\d+)-(\d+)" line))]
    [(Integer/parseInt (nth matches 1))
     (Integer/parseInt (nth matches 2))
     (Integer/parseInt (nth matches 3))
     (Integer/parseInt (nth matches 4))]))

(defn- is-overlapping [[a1, a2, b1, b2]]
  (or
   (and (>= a1 b1) (<= a2 b2)) ; a overlaps with b
   (and (>= b1 a1) (<= b2 a2)) ; b overlaps with a
   ))

(defn part-1
  "Day 04 part 1"
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       (map is-overlapping)
       (filter identity)
       count))

(defn is-overlapping2 [[a1, a2, b1, b2]]
  (or
   (and (<= a1 b1) (<= b1 a2))
   (and (<= a1 b2) (<= b2 a2))
   (and (<= b1 a1) (<= a1 b2))
   (and (<= b1 a2) (<= a2 b2))))

(defn part-2
  "day 04 part 2"
  [input]
  (->> input
       str/split-lines
       (map parse-line)
       (map is-overlapping2)
       (filter identity)
       count))

(comment
  (def test-input "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

  (def output (part-1 test-input))
  (= output 2)

  (def output2 (part-2 test-input))
  output2
  (= output2 4))
