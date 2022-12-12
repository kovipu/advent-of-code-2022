(ns day12.solution-12
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

(defn parse-width [input]
  (-> input str/split-lines first count))

(defn parse-height [input]
  (-> input str/split-lines count))

(defn map-elevation [e]
  (case e
    \S 1
    \E 26
    (- (int e) 96)))

(defn flatten [input]
  (str/replace input #"\n" ""))

(defn parse-elevations [input]
  (->> input
       (flatten)
       (map map-elevation)))

(defn find-idx [input c]
  (-> input
      flatten
      (str/index-of c)))

(defn parse [input]
  (let [width (parse-width input)
        height (parse-height input)
        elevations (parse-elevations input)
        start (find-idx input "S")
        end (find-idx input "E")]
    {:width width
     :height height
     :elevations elevations
     :start start
     :end end}))

(defn find-neighbors [{:keys [width height]} idx]
  (let [x (mod idx width)
        y (quot idx width)]
    (->> [(if (> x 0) (dec idx) nil)
          (if (< x (dec width)) (inc idx) nil)
          (if (> y 0) (- idx width) nil)
          (if (< y (dec height)) (+ idx width) nil)]
         (remove nil?))))

(defn filter-accessible [{:keys [elevations]} visited idx neighbors]
  (let [value (nth elevations idx)]
    (->> neighbors
         (filter #(<= (nth elevations %) (inc value)))
         ;; filter only ones not visited 
         (filter (fn [v] (not (some #{v} visited))))
         (apply vector))))

(defn build-neighbor-pairs
  "Format's neighbors for priority-queue"
  [{:keys [elevations]} {:keys [num-steps]} neighbors]
  (map
   #(vector {:idx %
             :num-steps (inc num-steps)}
            (as-> elevations e
              (nth e %)
              (- 27 e) ; invert the height to get priority.
              (+ e num-steps)) ; add steps to increase prio of short routes.
            )
   neighbors))

(comment
  (def heightmap (parse test-input))
  (def neighbors (find-neighbors heightmap 23))
  neighbors
  (def visited [0 1 8 2 10 9 10 17 18 16 25 24 10 9 8 26 34 35 36 37 38 39 31])
  (def accessible (filter-accessible heightmap visited
                                     23 neighbors))
  accessible
  (def current {:idx 34 :num-steps 12})
  (let [{:keys [elevations]} heightmap] (println elevations))
  accessible
  (build-neighbor-pairs heightmap current accessible))

;; {:visited [0, 1 ,2]
;;  :priority-queue { { :idx idx :num-steps num-steps } (27 - height) }}


(defn find-highest [heightmap visited priority-queue]
  (let [[current _priority] (peek priority-queue)
        {:keys [idx num-steps]} current]
    (if (= idx (:end heightmap))
      ;; found the goal, end recursion here
      num-steps
      ;; else, continue searching
      (let [neighbors (->> idx
                           (find-neighbors heightmap)
                           (filter-accessible heightmap visited idx))
            priority-pairs (build-neighbor-pairs heightmap current neighbors)
            new-visited (conj visited idx)
            new-priority-queue (-> priority-queue pop (into priority-pairs))]
        (find-highest heightmap new-visited new-priority-queue)))))


(comment
  (def heightmap (parse test-input))
  (def start {:idx 0 :num-steps 0})
  (def pq (priority-map start 0))
  (find-highest
   heightmap
   []
   pq))


(defn part-1
  "Day 12 part 1"
  [input]
  (let [heightmap (parse input)
        start {:idx (:start heightmap)
               :num-steps 0}]
    (find-highest
     heightmap
     []
     (priority-map start 0))))

(defn part-2
  "Day 12 part 2"
  [input]
  -1)

(comment
  (def test-input "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi")

  (def test-output (part-1 test-input))
  test-output
  )