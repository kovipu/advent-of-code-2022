(ns day14.solution-14
  (:require [clojure.string :as str]))

(defn parse-pair [pair]
  (let [[x y] (str/split pair #",")]
    {:x (Integer/parseInt x)
     :y (Integer/parseInt y)}))

(defn parse-line [line]
  (as-> line l
    (str/split l #" -> ")
    (map parse-pair l)
    (partition 2 1 l)))

(defn parse [input]
  (->> input
       str/split-lines
       (map parse-line)))

(defn add-rockline [rockmap [start end]]
  (let [[x1 x2] (sort [(:x start) (:x end)])
        [y1 y2] (sort [(:y start) (:y end)])]
    (if (= x1 x2)
      ;; if same x -> range from y start to y end 
      (reduce
       (fn [acc y]
         (conj acc {:x x1
                    :y y}))
       rockmap
       (range y1 (inc y2)))
      ;; if same y -> range from x start to x end
      (reduce
       (fn [acc x]
         (conj acc {:x x
                    :y y1}))
       rockmap
       (range x1 (inc x2))))))


(defn form-rocks [rockmap coords]
  (reduce add-rockline rockmap coords))

(defn find-bottom [rockmap]
  (reduce
   (fn [acc next]
     (let [y (:y next)]
       (if (> y acc) y acc)))
   0
   rockmap))

(defn drop-sand [state bottom x y]
  (loop [state state
         bottom bottom
         x x
         y y]
    ;; drop a sand from sand-coords
    ;; drop another sand when it lands.
    ;; stop when first sand falls into the abyss.
    (cond
      ;; sand fell into abyss?
      (> y bottom) state

      ;; empty space right under?
      (not (state {:x x :y (inc y)})) (recur state bottom x (inc y))

      ;; empty space down and to the left?
      (not (state {:x (dec x) :y (inc y)})) (recur state bottom (dec x) (inc y))

      ;; empty space down and to the right?
      (not (state {:x (inc x) :y (inc y)})) (recur state bottom (inc x) (inc y))

      ;; sand comes to rest -> drop another one
      :else (recur (conj state {:x x :y y}) bottom 500 0))))

(defn part-1
  "Day 14 part 1"
  [input]
  (let [rockmap (->> input
                     parse
                     (reduce form-rocks #{}))
        bottom (find-bottom rockmap)
        sand-filled (drop-sand rockmap bottom 500 0)]
    (- (count sand-filled) (count rockmap))))

(defn drop-sand-2 [state x y]
  (loop [state state
         x x
         y y]
    ;; drop a sand from sand-coords
    ;; drop another sand when it lands.
    ;; stop when full
    (cond
      ;; empty space right under?
      (not (state {:x x :y (inc y)})) (recur state x (inc y))

      ;; empty space down and to the left?
      (not (state {:x (dec x) :y (inc y)})) (recur state (dec x) (inc y))

      ;; empty space down and to the right?
      (not (state {:x (inc x) :y (inc y)})) (recur state (inc x) (inc y))

      ;; sand comes to rest
      :else
      (if (= y 0)
        ;; we are full -> return state
        (conj state {:x x :y y})
        ;; not full -> drop another sand
        (recur (conj state {:x x :y y}) 500 0)))))

(defn part-2
  "Day 14 part 2"
  [input]
  (let [rockmap (->> input
                     parse
                     (reduce form-rocks #{}))
        bottom (+ (find-bottom rockmap) 2)
        ;; add the floor
        rockmap (add-rockline
                 rockmap
                 [{:x -1
                   :y bottom}
                  {:x 1001
                   :y bottom}])
        
        sand-filled (drop-sand-2 rockmap 500 0)]
    (- (count sand-filled) (count rockmap))))

(comment
  (def inp "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9")

  (part-1 inp)
  (part-2 inp))