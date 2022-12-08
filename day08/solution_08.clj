(ns day08.solution-08
  (:require [clojure.string :as str]))

(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map #(->> (str/split % #"")))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn is-visible-col [col value]
  (->> col
       (every? #(< % value))))

(defn is-visible
  [grid y x]
  (let [value (-> grid (nth y) (nth x))
        up (->> grid
                (take y)
                (map #(nth % x)))
        down (->> grid
                  (drop (inc y))
                  (map #(nth % x)))
        left (->> (nth grid y)
                  (take x))
        right (->> (nth grid y)
                   (drop (inc x)))]
    (or (is-visible-col up value)
        (is-visible-col down value)
        (is-visible-col left value)
        (is-visible-col right value))))

(defn part-1
  "Day 08 part 1"
  [input]
  (let [grid (parse-input input)
        height (count grid)
        width (count (first grid))
        outside-trees (-
                       (* height width)
                       (* (- height 2) (- width 2)))]
    ;; outside trees are always visible.
    ;; iterate over each inside tree,
    ;; find if they're visible from any side
    (+ outside-trees
       (->> (for [y (range 1 (dec height))
                  x (range 1 (dec width))]
              (is-visible grid y x))
            (filter identity)
            count))))

(defn part-2
  "Day 08 part 2"
  [input]
  -1)

(comment
  (def test-input "30373
25512
65332
33549
35390")

  (def test-output (part-1 test-input))
  test-output
  (= test-output 21))
