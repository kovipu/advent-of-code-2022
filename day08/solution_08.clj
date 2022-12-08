(ns day08.solution-08
  (:require [clojure.string :as str]))


;; -- Common helpers -- ;;
(defn parse-input
  [input]
  (->> input
       str/split-lines
       (map #(->> (str/split % #"")))
       (map (fn [line] (map #(Integer/parseInt %) line)))))

(defn get-trees
  [grid y x]
  (let [up (->> grid
                (take y)
                (map #(nth % x))
                reverse)
        down (->> grid
                  (drop (inc y))
                  (map #(nth % x)))
        left (->> (nth grid y)
                  (take x)
                  reverse)
        right (->> (nth grid y)
                   (drop (inc x)))]
    [up down left right]))


;; -- Part 1 -- ;;
(defn is-visible-col
  [col value]
  (->> col
       (every? #(< % value))))

(defn is-visible
  [grid y x]
  (let [[up down left right] (get-trees grid y x)
        value (-> grid (nth y) (nth x))]
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


;; -- Part 2 -- ;;
(defn scenic-score-col
  [value coll]
  (let [visible-trees (->> coll
                           (take-while #(< % value))
                           count)]
    (if (= visible-trees (count coll))
      visible-trees
      (inc visible-trees))))

(defn scenic-score
  [grid y x]
  (let [[up down left right] (get-trees grid y x)
        value (-> grid (nth y) (nth x))]
    (* (scenic-score-col value up)
       (scenic-score-col value down)
       (scenic-score-col value left)
       (scenic-score-col value right))))

(defn part-2
  "Day 08 part 2"
  [input]
  (let [grid (parse-input input)
        height (count grid)
        width (count (first grid))]
    (apply max
           (for [y (range 1 (dec height))
                 x (range 1 (dec width))]
             (scenic-score grid y x)))))

(comment
  (def test-input "30373
25512
65332
33549
35390")

  (def test-output (part-1 test-input))
  test-output
  (= test-output 21)

  (def test-output2 (part-2 test-input))
  test-output2
  (= test-output2 8))
