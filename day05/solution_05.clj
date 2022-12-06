(ns day05.solution-05
  (:require [clojure.string :as str]))

(defn transpose [xs]
  (apply map list xs))

(defn parse-line [line]
  (let [chars (str/split line #"")]
    (->> chars
         (drop 1)
         (take-nth 4))))

(defn parse-stacks
  "Parse stacks into a computer-friendly format."
  [stacks]
  (->> stacks
       str/split-lines
       drop-last
       (map parse-line)
       transpose
       (map #(drop-while str/blank? %))))

(defn move-crates
  "Move crates according to the instruction."
  [stacks instruction reverse?]
  (let [regexp #"move (\d+) from (\d+) to (\d+)"
        [_all num from to] (re-find (re-matcher regexp instruction))
        num (Integer/parseInt num)
        from (dec (Integer/parseInt from))
        to (dec (Integer/parseInt to))
        from-col (nth stacks from)
        to-col (nth stacks to)
        moved-crates (take num from-col)
        moved-crates (if reverse? (reverse moved-crates) moved-crates)]
    ;; remove <num> crates from <from> column
    ;; reverse the order of the removed creates
    ;; push the crates to <to>
    (-> (apply vector stacks)
        (assoc from (drop num from-col))
        (assoc to (concat moved-crates to-col)))))

(defn solve-day [input reverse?]
  (let [[drawing instructions] (str/split input #"\n\n")
        stacks (parse-stacks drawing)]
    (->> instructions
         str/split-lines
         (reduce #(move-crates %1 %2 reverse?) stacks)
         (map first)
         (str/join ""))))

(defn part-1
  "Day 05 part 1"
  [input]
  (solve-day input true))

(defn part-2
  "Day 05 part 2"
  [input]
  (solve-day input false))

(comment
  (def test-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

  (def output (part-1 test-input))
  (= output "CMZ"))