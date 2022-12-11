(ns day11.solution-11
  (:require [clojure.string :as str]))

(defn parse-operation [line]
  (let [operation (as-> line l
                    (drop 22 l)
                    (apply str l)
                    (str/split l #" "))
        op (-> operation
               (nth 1)
               (case "*" * "+" +))
        b (-> operation (nth 2) str)
        resolve-b #(if (= b "old") % (read-string b))]
    (fn [old] (let [b (resolve-b old)]
                (op old b)))))

(defn parse-monkey [input]
  (let [lines (str/split-lines input)
        items (as-> (nth lines 1) line
                (drop 18 line)
                (apply str line)
                (str/split line #", ")
                (map read-string line))
        operation (parse-operation (nth lines 2))
        test (->> (nth lines 3)
                  (drop 21)
                  (apply str)
                  read-string)
        if-true (->> (nth lines 4)
                     (drop 29)
                     (apply str)
                     read-string)
        if-false (->> (nth lines 5)
                      (drop 30)
                      (apply str)
                      read-string)]
    (hash-map :items items
              :operation operation
              :test test
              :if-true if-true
              :if-false if-false
              :inspected 0)))


(defn take-turn [monkeys i]
  ;; for each item
  (let [{:keys [items operation test if-true if-false]} (nth monkeys i)]
    (as-> monkeys state
      ;; inspect and throw all items
      (reduce (fn [acc item]
                (let [worry-level (-> item (operation) (quot 3))
                      modulo (mod worry-level test)
                      monke-idx (if (= modulo 0) if-true if-false)
                      ;; add item to the monke
                      monke (-> (nth acc monke-idx)
                                (update-in [:items] #(conj % worry-level)))]
                  (assoc (into [] acc) monke-idx monke)))
              state
              items)
      ;; clear this monkey's items
      (assoc-in state [i :items] [])
      ;; up the inspected number
      (update-in state [i :inspected] #(+ % (count items))))))


(defn simulate-round [monkeys]
  (reduce
   (fn [acc i] (take-turn acc i))
   monkeys
   (range (count monkeys))))


(defn part-1
  "Day 11 part 1"
  [input]
  (let [monkeys (->> (str/split input #"\n\n")
                     (map parse-monkey)
                     simulate-round)]
    (as-> monkeys state
      (reduce (fn [acc _] (simulate-round acc)) state (range 19))
      (map :inspected state)
      (sort state)
      (take-last 2 state)
      (apply * state))))


(defn part-2
  "Day 11 part 2"
  [input]
  -1)

(comment
  (def test-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

  (def test-output (part-1 test-input))
  test-output
  (= test-output 10605))
