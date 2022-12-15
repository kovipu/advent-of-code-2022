(ns day15.solution-15
  (:require [clojure.string :as str]))

(defn manhattan-distance [a b]
  (+ (Math/abs (- (:x a) (:x b)))
     (Math/abs (- (:y a) (:y b)))))

(defn parse-line [line]
  (let [regexp #"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)"
        matches (re-find (re-matcher regexp line))
        [_all sx sy bx by] matches
        sensor {:x (Integer/parseInt sx)
                :y (Integer/parseInt sy)}
        beacon {:x (Integer/parseInt bx)
                :y (Integer/parseInt by)}]
    {:sensor sensor
     :beacon beacon
     :radius (manhattan-distance sensor beacon)}))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map parse-line)))

(defn block-beacon-area [blocked-area row bs]
  (let [{:keys [sensor radius]} bs
        {:keys [x y]} sensor
        distance (Math/abs (- row y))
        coverage (- radius distance)]
    ;; if coverage > 0 -> add blocked area
    ;; the bigger the coverage, wider the blocked area
    (if (>= coverage 0)
      (reduce conj blocked-area (range (- x coverage) (+ x coverage 1)))
      blocked-area)))

(defn find-blocked-spots [beacons row]
  ;; add the blocked area of each becon on row.
  (reduce #(block-beacon-area %1 row %2) #{} beacons))

(defn find-beacons-on-row [beacons row]
  (->> beacons
       (filter (fn [bs] (= row (:y (:beacon bs)))))
       (map #(:x (:beacon %)))
       set))

(defn part-1
  "Day 15 part 1"
  [input]
  (let [beacons (parse input)
        row 2000000
        blocked-spots (find-blocked-spots beacons row)
        beacons-on-row (find-beacons-on-row beacons row)
        empty-spots (remove #(some #{%} beacons-on-row) blocked-spots)]
    (count empty-spots)))

(defn part-2
  "Day 15 part 2"
  [input]
  -1)

(comment
  (def inp "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3")
  (parse inp)

  (part-1 inp))