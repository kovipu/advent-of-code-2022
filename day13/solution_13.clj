(ns day13.solution-13
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [lib.library :refer [with-index]]))

(defn parse-pair [input]
  (let [pairs (str/split input #"\n" 2)]
    (->> pairs
         (map edn/read-string)
         (apply vector))))

(defn parse [input]
  (as-> input i
    (str/split i #"\n\n")
    (map parse-pair i)))

(defn is-ordered? [left right]
  (cond
    (and (number? left) (number? right)) (if (= left right)
                                     :recurse-please
                                     (< left right))

    (number? left) (is-ordered? [left] right)

    (number? right) (is-ordered? left [right])

    (and (empty? left) (empty? right)) :recurse-please

    (empty? left) true

    (empty? right) false
  
    :else (let [pair-ordered? (is-ordered? (first left) (first right))]
            (if (= :recurse-please pair-ordered?)
              (is-ordered? (rest left) (rest right))
              pair-ordered?)))
  )

(defn part-1
  "Day 13 part 1"
  [input]
  (->> input
       parse
       with-index
       (reduce (fn [acc [idx [left right]]]
                 (if (is-ordered? left right)
                   (+ acc idx 1)
                   acc)) 0)))

(defn part-2
  "Day 13 part 2"
  [input]
  -1)

(comment
  (def input "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]")

  (part-1 input))