(ns day06.solution-06
  (:require [clojure.string :as str] [clojure.pprint :as pprint] ))

(defn find-index [f coll]
  (count (take-while (complement f) coll)))

(defn find-marker [line]
  (let [first-char (first line)
        line2 (str first-char line "..")
        line3 (str (apply str (repeat 2 first-char)) line ".")
        line4 (str (apply str (repeat 3 first-char)) line)
        zipped (map vector line line2 line3 line4)]
    (->> zipped
         (find-index #(apply distinct? %))
         inc)))

(defn part-1
  "Day 06 part 1"
  [input]
  (find-marker input))

(defn part-2
  "Day 06 part 2"
  [input]
  -1)

(comment
  (def test-line "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  (def test-line-output (find-marker test-line))
  (= test-line-output 7)
  
  (def test2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
  (def test2-output (find-marker test2))
  (= test2-output 5)
)