(ns lib.library
  (:require [clojure.string :as str]))

(defn print-map [width height coords]
  (let [m (for [y (range (- height) height)]
            (for [x (range (- width) width)]
              (let
               [idx (.indexOf coords {:x x :y y})]
                (if (= -1 idx)
                  "."
                  (str idx)))))]
    (->> m
         (map #(str/join "" %))
         (str/join "\n")
         println)))

(defn with-index [xs]
  (map vector (range) xs))

(comment
  (def coords [{:x 1 :y 1} {:x 2 :y 2}])
  (print-map 5 5 coords))