(ns advent-2018.day23
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [abs]]))

;;; https://adventofcode.com/2018/day/23

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Each line is of the form: pos=<x,y,z>, r=range.
;; So just pull out all separate numbers in this case. Note that some of the
;; position values can be negative.
(defn- parse-lines [lines]
  (loop [[line & lines] lines, bots []]
    (cond
      (nil? line) bots
      :else
      (recur lines
             (conj bots
                   (mapv #(Integer/parseInt %) (re-seq #"-?\d+" line)))))))

;; Calculate the Manhattan Distance between two points:
(defn- manhattan-dist [p1 p2]
  (apply + (map #(abs (- %1 %2)) (take 3 p1) (take 3 p2))))

;; Problem 1

;; Find the index and range of the bot with the greatest range. Return a map
;; with these two values, as we convert each one to a map for the sake of
;; sorting.
(defn- find-greatest-range [bots]
  (last (sort #(compare (:range %1) (:range %2))
              (map-indexed (fn [i n] {:index i, :range (last n)}) bots))))

;; Predicate-- is point p2 within the range r of point p1?
(defn- in-range? [p1 p2 r] (<= (manhattan-dist p1 p2) r))

;; Find the bot with the greatest range, then count the number of
;; bots (including itself) that are within its range.
(defn- nanobots-in-range [bots]
  (let [{idx :index, r :range} (find-greatest-range bots)
        ref-bot                (bots idx)]
    (count (filter identity (map #(in-range? ref-bot % r) bots)))))

;; The file contains the positions and ranges of all the nanobots. For this
;; problem, find the one with the greatest range and count the total number
;; of bots within that range (including the one with the range).
(defn p01 [file]
  (->> file
       (read-lines)
       (parse-lines)
       (nanobots-in-range)))

;; Problem 2

;; Remove the element at pos from the vector v.
(defn- pop-at [v pos]
  (vec (concat (subvec v 0 pos) (subvec v (inc pos)))))

;; Find the best candidate from the vector passed in. Need to return both the
;; target value and its index, as the index may be used to remove it from the
;; vector if it proves to be a false path.
(defn- find-best [coll]
  (first (sort #(compare (last %1) (last %2))
               (map-indexed (fn [i n] {:index i, :value n}) coll))))

;; Based on target, find the coordinates that have bots within range of
;; each other and count the number of bots that have the coordinate within
;; their range. Return a collection of these, where each element is
;; [x y z count-of-bots distance-to-origin].
(defn- find-range [bots xs ys zs dist off-x off-y off-z target]
  (filterv identity
           (for [x (range (apply min xs) (inc (apply max xs)) dist)
                 y (range (apply min ys) (inc (apply max ys)) dist)
                 z (range (apply min zs) (inc (apply max zs)) dist)]
             (loop [[bot & bots] bots, cnt 0]
               (cond
                 (nil? bot) (when (>= cnt target)
                              [x y z cnt (+ (abs x) (abs y) (abs z))])
                 (= 1 dist) (let [[bx by bz bdist] bot
                                  calc             (+ (abs (- x bx))
                                                      (abs (- y by))
                                                      (abs (- z bz)))]
                              (if (<= calc bdist)
                                (recur bots (inc cnt))
                                (recur bots cnt)))
                 :else
                 (let [[bx by bz bdist] bot
                       calc             (+ (abs (- (+ off-x x)
                                                   (+ off-x bx)))
                                           (abs (- (+ off-y y)
                                                   (+ off-y by)))
                                           (abs (- (+ off-z z)
                                                   (+ off-z bz))))]
                   (if (<= (- (quot calc dist) 3) (quot bdist dist))
                     (recur bots (inc cnt))
                     (recur bots cnt))))))))

;; Perform a tree-based search/find, gathering the best count and distance
;; for the set of target-coords returned by find-range.
(defn- tree-find [bots xs ys zs dist off-x off-y off-z target]
  (let [at-target (find-range bots xs ys zs dist off-x off-y off-z target)]
    (loop [at-target at-target, a nil, b nil]
      (cond
        (empty? at-target) (list a b)
        :else
        (let [{best   :value
               best-i :index} (find-best at-target)]
          (cond
            (= 1 dist) (recur [] (best 4) (best 3))
            :else
            (let [xs (list (best 0) (+ (best 0) (quot dist 2)))
                  ys (list (best 1) (+ (best 1) (quot dist 2)))
                  zs (list (best 2) (+ (best 2) (quot dist 2)))
                  [a b] (tree-find bots
                                   xs ys zs
                                   (quot dist 2)
                                   off-x off-y off-z
                                   target)]
              (cond
                (nil? a) (recur (pop-at at-target best-i) a b)
                :else    (recur [] a b)))))))))

;; Find the total Manhattan distance for the closest point with the most
;; in-range bots. Adaped from a description (and some parts of a Python
;; implementation) posted to reddit.
(defn- find-shortest-range [bots]
  (let [xs    (cons 0 (map #(nth % 0) bots))
        ys    (cons 0 (map #(nth % 1) bots))
        zs    (cons 0 (map #(nth % 2) bots))
        min-x (apply min xs)
        max-x (apply max xs)
        min-y (apply min ys)
        max-y (apply max ys)
        min-z (apply min zs)
        max-z (apply max zs)
        dist  (loop [dist 1]
                (cond
                  (or (< dist (- max-x min-x))
                      (< dist (- max-y min-y))
                      (< dist (- max-z min-z))) (recur (* dist 2))
                  :else                         dist))
        off-x (- min-x)
        off-y (- min-y)
        off-z (- min-z)
        span  (loop [span 1]
                (cond
                  (< span (count bots)) (recur (* span 2))
                  :else                 span))]
    (loop [target 1, span span, tried {}, best-val nil, best-count -1]
      (let [tried                 (if (contains? tried target)
                                    tried
                                    (assoc tried target
                                           (tree-find bots xs ys zs dist
                                                      off-x off-y off-z
                                                      target)))
            [test-val test-count] (get tried target)]
        (cond
          (nil? test-val) (let [span   (if (> span 1) (quot span 2) span)
                                target (max 1 (- target span))]
                            (recur target span tried best-val best-count))
          :else
          (let [[best-val best-count] (if (> test-count best-count)
                                        (list test-val test-count)
                                        (list best-val best-count))]
            (cond
              (= span 1) best-val
              :else
              (recur (+ target span) span tried best-val best-count))))))))

;; Using the sets of postitions and ranges, determine the distance from origin
;; of the nearest point that is within range of the largest number of bots.
(defn p02 [file]
  (->> file
       (read-lines)
       (parse-lines)
       (find-shortest-range)))
