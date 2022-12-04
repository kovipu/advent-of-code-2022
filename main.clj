(ns main (:require [day01.solution-01 :as d01]
                   [day02.solution-02 :as d02]
                   [day03.solution-03 :as d03]
                   [day04.solution-04 :as d04]))

(defn- read-input [day]
  (let [d (format "%02d" day)
        filename (str "day" d "/input_" d ".txt")]
    (slurp (clojure.java.io/resource filename))))

(defn- solve-day [day]
  (let [dayFns
        {1 [d01/part-1 d01/part-2]
         2 [d02/part-1 d02/part-2]
         3 [d03/part-1 d03/part-2]
         4 [d04/part-1 d04/part-2]}
        [part1 part2] (dayFns day)
        input (read-input day)]
    (println (str "\n-- Day " day " --"))
    (print "Part 1: ")
    (println (part1 input))
    (print "Part 2: ")
    (println (part2 input))))

(defn -main
  "Main function called from the command line."
  [& args]
  (dotimes [day 4] (solve-day (+ day 1))))
