(ns day06.solution-06)

(defn find-index [f coll]
  (count (take-while (complement f) coll)))

(defn gen-lines [line num-lines]
  (let [first-char (first line)]
    (map
     ;; repeat the first character n times.
     #(str (apply str (repeat % first-char)) line)
     (range num-lines))))

(defn find-marker [line num-lines]
  (let [lines (gen-lines line num-lines)
        zipped (apply map vector lines)]
    (->> zipped
         (find-index #(apply distinct? %))
         inc)))

(defn part-1
  "Day 06 part 1"
  [input]
  (find-marker input 4))

(defn part-2
  "Day 06 part 2"
  [input]
  (find-marker input 14))

(comment
  (def test-line "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
  (def test-line-output (find-marker test-line 4))
  (= test-line-output 7)

  (def test2 "bvwbjplbgvbhsrlpgdmjqwftvncz")
  (def test2-output (find-marker test2 4))
  (= test2-output 5)

  (apply str (repeat 0 ".")))