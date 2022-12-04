(ns main (:require [day01.solution-01 :as d01]))

(defn- read-input
  [day]
  (slurp (clojure.java.io/resource day)))

(defn -main
  "Main function called from the command line."
  [& args]
  (let [input (read-input "day01/input_01.txt")]
    (println "-- Day 01 --")
    (print "Part 1:  ")
    (println (d01/part-1 input))
    (print "Part 2:  ")
    (println (d01/part-2 input))))
