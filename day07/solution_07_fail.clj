;; This is a failed attempt.
;; I was trying to be too clever with the str/split #"$ cd "
;; It works for the test input, but not for the actual input.
;; The cause is a recursion loop because there are folders with matching names.
(ns day07.solution-07-fail
  (:require [clojure.string :as str]))

(defn parse-int [num]
  (try
    (Integer/parseInt num)
    (catch NumberFormatException _ 0)))

(defn find-dirsize [dir]
  (let [dir-name (first dir)
        size (->> dir
                  (map #(-> % (str/split #" ") first parse-int))
                  (reduce + 0))]
    [(keyword dir-name) size]))

(defn add-recursive-dirs [dirs dirsizes current-dir]
  ;; if 2 same name directories are relative...
  (let [child-dirs (->> dirs
                        (filter #(= current-dir (first %)))
                        first
                        (filter #(str/starts-with? % "dir"))
                        (map #(apply str (drop 4 %))))
        own-size (dirsizes (keyword current-dir))]
    (->> child-dirs
         (map #(add-recursive-dirs dirs dirsizes %))
         (reduce + own-size))))

(defn find-filesizes
  "Find the size of the files in a directory"
  [input]
  (let [dirs (->> (str/split input #"\$ cd ")
                  (filter #(not (or (str/starts-with? % "$") (str/starts-with? % ".."))))
                  (map str/split-lines))
        dir-sizes (->> dirs (map find-dirsize) (into {}))
        dir-names (map first dirs)]
    (->> dir-names
         (map #(add-recursive-dirs dirs dir-sizes %))
         (filter #(< % 100000))
         (reduce +))))

(defn part-1
  "Day 07 part 1"
  [input]
  (->> input
       find-filesizes)),

(defn part-2
  "Day 07 part 2"
  [input]
  -1)

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
  (= test-output 95437))