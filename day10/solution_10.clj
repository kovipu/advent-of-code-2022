(ns day10.solution-10
  (:require [clojure.string :as str]))

(defn parse-line [acc line]
  (let [instruction (subs line 0 4)]
    (case instruction
      "noop" (concat acc (vector (last acc)))
      "addx" (let [v (Integer/parseInt (subs line 5))
                   state (+ (last acc) v)]
               (concat acc [state state])))))

(defn parse-states [input]
  (->> input
       str/split-lines
       (reduce parse-line [1])))

(defn part-1
  "Day 10 part 1"
  [input]
  (let [states (parse-states input)
        state40s (take-nth 40 (drop 18 states))]
    (->> state40s
         (map-indexed
          (fn [i n]
            (->> i
                 (* 40)
                 (+ 20)
                 (* n))))
         (reduce +))))

(defn render-line [line]
  (->> line
       (map-indexed
        (fn [i n]
          ;; if i is close to n, render #
          (if (<= (dec n) i (inc n))
            "#"
            ".")))
       (str/join "")))

(defn part-2
  "Day 10 part 2"
  [input]
  (->> input
       parse-states
       (into [1])
       (partition 40)
       (map render-line)
       (str/join "\n")
       (str "\n")))


(comment
  (def test-input "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

  (def test-output (part-1 test-input))
  (= test-output 13140)
  
  (println (part-2 test-input))
  )
