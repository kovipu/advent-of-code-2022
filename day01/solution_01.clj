(ns day01.solution-01
  (:require [clojure.string :as str]))


(defn- get-elf-lines [input]
  (str/split input #"\n\n"))

(defn- calc-elf-load [input]
  (->> input
       str/split-lines
       (reduce (fn [acc number] (+ acc (Integer/parseInt number))) 0)))

(defn part-1
  "Day 01 part 1"
  [input]
  (->> input
       (get-elf-lines)
       (map calc-elf-load)
       (apply max)))

(defn part-2
  "Day 01 part 2"
  [input]
  (->> input
       get-elf-lines
       (map calc-elf-load)
       sort
       reverse
       (take 3)
       (apply +)))

(comment
  ;; Load the example input.
  (def test-input "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

  ;; Assert the example input works.
  (def output (part-1 test-input))
  (def output2 (part-2 test-input))
  (= output 24000)
  (= output2 45000)

  nil)
