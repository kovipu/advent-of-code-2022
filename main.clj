(ns main (:require [day01.solution-01 :as d01]))

(defn- read-input
  [day]
  (slurp (clojure.java.io/resource day)))
  
(defn -main
  "Main function called from the command line."
  [& args]
  (println (d01/part-1 (read-input "day01/input_01.txt"))))
