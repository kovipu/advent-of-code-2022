(ns day09.solution-09
  (:require [clojure.string :as str]))

(defn run-step [acc [direction step]]
  (let [[head-coords tail-coords tail-locations] acc
        x (:x head-coords)
        y (:y head-coords)
        tailX (:x tail-coords)
        tailY (:y tail-coords)
        newX (case direction
               \L (dec x)
               \R (inc x)
               x)
        newY (case direction
               \U (inc y)
               \D (dec y)
               y)
        tail-lagging (or
                      (> (Math/abs (- newX tailX)) 1)
                      (> (Math/abs (- newY tailY)) 1))
        new-tail-coords (if tail-lagging
                          {:x (cond
                                (< tailX newX) (inc tailX)
                                (> tailX newX) (dec tailX)
                                :else tailX)
                           :y (cond
                                (< tailY newY) (inc tailY)
                                (> tailY newY) (dec tailY)
                                :else tailY)}
                          tail-coords)]
    ;; move head to direction.
    ;; if tail is more than one behind -> move
    ;; add new tail-location
    [{:x newX
      :y newY}
     new-tail-coords
     (conj tail-locations new-tail-coords)]))

(defn run-instruction [acc instruction]
  (let [direction (first instruction)
        steps (->> instruction (drop 2) (str/join "") (Integer/parseInt))]
    ;; change head-coords once per step
    ;; change tail-coord accordingly
    ;; add new tail-coord to locations
    (reduce
     #(run-step %1 [direction %2])
     acc
     (for [step (range steps)] step))))

(defn part-1
  "Day 09 part 1"
  [input]
  ;; head position: head-coords
  ;; tail position: [head-coords tail-locations]
  (-> (->> input
           str/split-lines
           (reduce
            run-instruction
            [{:x 0 :y 0}
             {:x 0 :y 0}
             #{}]))
      (nth 2)
      count))

(defn part-2
  "Day 09 part 2"
  [input]
  -1)

(comment

  (def test-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")
  
  (def test-output (part-1 test-input))
  (= test-output 13))