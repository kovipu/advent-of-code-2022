(ns day09.solution-09
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> input
       str/split-lines
       (map #(hash-map
              :direction (first %)
              :num-steps (->> %
                              (drop 2)
                              (str/join "")
                              Integer/parseInt)))))

(defn move-tail [head tail]
  (let [headX (:x head)
        headY (:y head)
        tailX (:x tail)
        tailY (:y tail)
        tail-lagging (or
                      (> (Math/abs (- headX tailX)) 1)
                      (> (Math/abs (- headY tailY)) 1))]
    (if tail-lagging
      {:x (cond
            (< tailX headX) (inc tailX)
            (> tailX headX) (dec tailX)
            :else tailX)
       :y (cond
            (< tailY headY) (inc tailY)
            (> tailY headY) (dec tailY)
            :else tailY)}
      tail)))

(defn run-step [acc [direction _]]
  (let [[head-coords tail-coords tail-locations] acc
        x (:x head-coords)
        y (:y head-coords)
        newX (case direction
               \L (dec x)
               \R (inc x)
               x)
        newY (case direction
               \U (inc y)
               \D (dec y)
               y)
        new-tail (move-tail {:x newX :y newY} tail-coords)]
    ;; move head to direction.
    ;; if tail is more than one behind -> move
    ;; add new tail-location
    [{:x newX
      :y newY}
     new-tail
     (conj tail-locations new-tail)]))

(defn run-instruction [acc instruction]
  (let [direction (:direction instruction)
        steps (:num-steps instruction)]
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
  (-> (->> input
           parse
           (reduce
            run-instruction
            [{:x 0 :y 0} ;; head position
             {:x 0 :y 0} ;; tail position
             #{} ;; unique tail locations´
             ]))
      (nth 2)
      count))


(defn run-step2 [acc direction _]
  ;; move first one once,
  ;; check all others if they need to be moved
  (let [[rope tail-locations] acc
        head (first rope)
        x (:x head)
        y (:y head)
        newX (case direction
               \L (dec x)
               \R (inc x)
               x)
        newY (case direction
               \U (inc y)
               \D (dec y)
               y)
        new-rope (assoc (into [] rope) 0 {:x newX :y newY})
        tail-moved (reduce
                    (fn [rope next]
                      (conj rope (move-tail (last rope) next)))
                    [(first new-rope)]
                    (drop 1 new-rope))]
    [tail-moved (conj tail-locations (last tail-moved))]))

(defn run-instruction2 [acc instruction]
  (let [direction (:direction instruction)
        num-steps (:num-steps instruction)]
    (reduce
     #(run-step2 %1 direction %2)
     acc
     (for [step (range num-steps)] step))))

(defn part-2
  "Day 09 part 2"
  [input]
  ;; head-position
  ;; 9 tails in an array
  ;; unique locations of the last one in a set.
  (->
   (->> input
        parse
        (reduce
         run-instruction2
         [(for [_ (range 10)] {:x 0 :y 0}) ;; rope position
          #{} ;; unique tail locations
          ]))
   last
   count))

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
  test-output
  (= test-output 13)

  (def test-input2 "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

  (def test-output2 (part-2 test-input2))
  test-output2
  (= test-output2 36)
  test-output2

  (def rope (lazy-seq [{:x 1 :y 2} {:x 2 :y 5}]))
  rope
  (assoc (into [] rope) 0 {:x -1 :y -1}))