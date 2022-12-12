(ns day12.solution-12
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as str]
            [lib.library :refer [with-index]]))

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
       flatten
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

(defn find-highest [heightmap visited priority-queue]
  (if (empty? priority-queue)
    nil
    (let [[current _prioriy] (peek priority-queue)
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
          (find-highest heightmap new-visited new-priority-queue))))))

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

;; commented because slow
;; (defn part-2
;;   "Day 12 part 2"
;;   [input]
;;   (let [heightmap (parse input)
;;         starts (->> heightmap
;;                     :elevations
;;                     with-index
;;                     (filter (fn [[_i v]] (= v 1)))
;;                     (map first))]
;;     (->> (for [start starts]
;;            (find-highest
;;             heightmap
;;             []
;;             (priority-map
;;              {:idx start
;;               :num-steps 0}
;;              0)))
;;          (remove nil?)
;;          (apply min))))
(defn part-2 [_input] -1)

(comment
  (def test-input "abaacccccccccccccaaaaaaaccccccccccccccccccccccccccccccccccaaaaaa
abaaccccccccccccccaaaaaaaaaaccccccccccccccccccccccccccccccccaaaa
abaaaaacccccccccaaaaaaaaaaaaccccccccccccccccccccccccccccccccaaaa
abaaaaaccccccccaaaaaaaaaaaaaacccccccccccccccccdcccccccccccccaaaa
abaaaccccccccccaaaaaaaaccacacccccccccccccccccdddcccccccccccaaaaa
abaaacccccccccaaaaaaaaaaccaaccccccccccccciiiiddddcccccccccccaccc
abcaaaccccccccaaaaaaaaaaaaaaccccccccccciiiiiijddddcccccccccccccc
abccaaccccccccaccaaaaaaaaaaaacccccccccciiiiiijjddddccccaaccccccc
abccccccccccccccaaacaaaaaaaaaaccccccciiiiippijjjddddccaaaccccccc
abccccccccccccccaacccccaaaaaaacccccciiiippppppjjjdddddaaaaaacccc
abccccccccccccccccccccaaaaaaccccccckiiippppppqqjjjdddeeeaaaacccc
abccccccccccccccccccccaaaaaaccccckkkiippppuupqqjjjjdeeeeeaaccccc
abccccccccccccccccccccccccaaccckkkkkkipppuuuuqqqjjjjjeeeeeaccccc
abccccccccccccccccccccccccccckkkkkkoppppuuuuuvqqqjjjjjkeeeeccccc
abcccccccccccccccccccccccccckkkkooooppppuuxuvvqqqqqqjkkkeeeecccc
abccaaccaccccccccccccccccccckkkoooooopuuuuxyvvvqqqqqqkkkkeeecccc
abccaaaaacccccaaccccccccccckkkoooouuuuuuuxxyyvvvvqqqqqkkkkeecccc
abcaaaaacccccaaaacccccccccckkkooouuuuxxxuxxyyvvvvvvvqqqkkkeeeccc
abcaaaaaaaaaaaaacccccccccccjjjooottuxxxxxxxyyyyyvvvvrrrkkkeecccc
abcccaaaacaaaaaaaaacaaccccccjjoootttxxxxxxxyyyyyyvvvrrkkkfffcccc
SbccaacccccaaaaaaaaaaaccccccjjjooottxxxxEzzzyyyyvvvrrrkkkfffcccc
abcccccccccaaaaaaaaaaaccccccjjjooootttxxxyyyyyvvvvrrrkkkfffccccc
abcaacccccaaaaaaaaaaaccccccccjjjooottttxxyyyyywwvrrrrkkkfffccccc
abaaacccccaaaaaaaaaaaaaacccccjjjjonnttxxyyyyyywwwrrlllkfffcccccc
abaaaaaaaaaaacaaaaaaaaaaccccccjjjnnnttxxyywwyyywwrrlllffffcccccc
abaaaaaaaaaaaaaaaaaaaaaaccccccjjjnntttxxwwwwwywwwrrlllfffccccccc
abaaccaaaaaaaaaaaaaaacccccccccjjjnntttxwwwsswwwwwrrlllfffccccccc
abaacccaaaaaaaacccaaacccccccccjjinnttttwwsssswwwsrrlllgffacccccc
abccccaaaaaaccccccaaaccccccccciiinnntttsssssssssssrlllggaacccccc
abccccaaaaaaaccccccccccaaccccciiinnntttsssmmssssssrlllggaacccccc
abccccaacaaaacccccccaacaaaccccciinnnnnnmmmmmmmsssslllgggaaaacccc
abccccccccaaacccccccaaaaacccccciiinnnnnmmmmmmmmmmllllgggaaaacccc
abaaaccccccccccccccccaaaaaacccciiiinnnmmmhhhmmmmmlllgggaaaaccccc
abaaaaacccccccccccaaaaaaaaaccccciiiiiiihhhhhhhhmmlgggggaaacccccc
abaaaaaccccaaccccaaaaaaacaacccccciiiiihhhhhhhhhhggggggcaaacccccc
abaaaaccccaaaccccaaaacaaaaacccccccciiihhaaaaahhhhggggccccccccccc
abaaaaaaacaaacccccaaaaaaaaaccccccccccccccaaaacccccccccccccccccaa
abaacaaaaaaaaaaaccaaaaaaaaccccccccccccccccaaaccccccccccccccccaaa
abcccccaaaaaaaaacccaaaaaaaccccccccccccccccaacccccccccccccccccaaa
abccccccaaaaaaaaaaaaaaaaacccccccccccccccccaaacccccccccccccaaaaaa
abcccccaaaaaaaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccaaaaaa")

  (def test-output (part-1 test-input))
  test-output

  (def heightmap (parse test-input))
  (->> heightmap
       :elevations
       with-index
       (filter (fn [[i v]] (= v 1)))
       (map first))
  (def pq (priority-map {:idx 17 :num-steps 0} 0))
  (find-highest heightmap [] pq)

  (def test-output2 (part-2 test-input))
  test-output2)
