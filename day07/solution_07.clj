(ns day07.solution-07
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]))

;; Acc [
;;   current-dir [:/]
;;   filetree {
;;     :a { ... }
;;     :b.txt 5000
;;   }
;; ]
(defn line-reducer
  [[current-dir filetree] line]
  (cond
    ;; go up a directory
    (= line "$ cd ..")
    [(apply vector (drop-last 1 current-dir)) filetree]

    ;; change directory
    (str/starts-with? line "$ cd")
    [(->> line (drop 5) (str/join "") keyword (conj current-dir))
     filetree]

    ;; list
    (= line "$ ls")
    [current-dir filetree]

    ;; new directory
    (str/starts-with? line "dir ")
    [current-dir
     (assoc-in filetree (->> line (drop 4) (str/join "") keyword (conj current-dir)) {})]

    ;; new file
    :else
    (let [[filesize, filename] (str/split line #" ")]
      [current-dir
       (assoc-in filetree (->> filename keyword (conj current-dir)) filesize)])))


(defn build-filetree [input]
  (->> input
       str/split-lines
       (reduce line-reducer [[] {}])))

(defn find-directory-sizes [filetree]
  ;; call each child and return their [new-tree size]
  ;; place new-tree as their new value
  ;; add own size + childs' sizes
  (if (empty? filetree) {:size 0}
      (reduce
       (fn [acc [k v]]
         (let [old-size (get acc :size 0)]
           (try
             (let [num (Integer/parseInt v)]
               (-> acc
                   (assoc k num)
                   (assoc :size (+ num old-size))))
             (catch ClassCastException _
               (let [new-child (find-directory-sizes v)]
                 (-> acc
                     (assoc k new-child)
                     (assoc :size (+ old-size (new-child :size))))))
             (catch NullPointerException _ (assoc acc :size 0)))))
       filetree
       (seq filetree))))

;; borrowed code
;; https://clojureverse.org/t/how-to-transform-nested-map-into-flat-sequence-of-path-value-pairs/8801/2
(defn recover [xs]
  (reduce
   (fn [acc x]
     (if  (or (vector? x) (not (seq x)))
       (into acc x)
       (into acc (recover x))))
   [] xs))

(defn normalize
  ([path m]
   (if (map? m)
     (for [[k v] m]
       (normalize (conj path k) v))
     [path m]))
  ([m] (-> (normalize [] m)
           recover)))

(defn zip-list [ls]
  (map vector
       (take-nth 2 ls)
       (take-nth 2 (drop 1 ls))))

(defn part-1
  "Day 07 part 1"
  [input]
  (let [filetree (-> input build-filetree (nth 1))]
    (->> filetree
         find-directory-sizes
         normalize
         zip-list
         (filter
          (fn [[path size]]
            (and (= (last path) :size)
                 (< size 100000))))
         (map #(nth % 1))
         (reduce +))))

(def space-total 70000000)
(def space-required 30000000)

(defn part-2
  "Day 07 part 2"
  [input]
  (let [filetree (-> input build-filetree (nth 1))
        filesizes (->> filetree
                       find-directory-sizes
                       normalize
                       zip-list)
        space-used (- space-total (last (last filesizes)))
        space-needed (- space-required space-used)]
    (->> filesizes
         (filter (fn [[path size]]
                   (and (= (last path) :size)
                        (> size space-needed))))
         (map #(nth % 1))
         (apply min))))

(comment
  (def test-input "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

  (def test-output (part-1 test-input))
  test-output
  (= test-output 95437)

  (def test-output2 (part-2 test-input))
  (= test-output2 24933642))