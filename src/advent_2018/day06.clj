(ns advent-2018.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.math.numeric-tower :refer [abs]]))

;;; https://adventofcode.com/2018/day/6

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Turn one text-line into a coordinate pair:
(defn- line-to-pair [line]
  (map #(Integer/parseInt %) (rest (re-find #"(\d+),\s+(\d+)" line))))

;; Convert all the lines of text into a list of coordinate pairs:
(defn- lines-to-pairs [lines]
  (map line-to-pair lines))

;; Create a field based on the size that the full set of pairs requires.
;; Initialize it to all -1 values.
(defn- create-field [pairs]
  (let [max-x (apply max (map #(nth % 1) pairs))
        max-y (apply max (map last pairs))]
    (vec (repeat (inc max-y) (vec (repeat (inc max-x) -1))))))

;; Calculate the Manhattan Distance between two points:
(defn- manhattan-dist [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (last  p1) (last  p2)))))

;; Problem 1

;; Find the coordinate that is nearest to this (x, y) point in the field.
(defn- find-nearest [x y pairs]
  (let [pairs (vec pairs)
        base  (list x y)
        num   (count pairs)
        dists (sort #(compare (:dist %1) (:dist %2))
                    (map #(hash-map :idx %
                                    :dist (manhattan-dist base (pairs %)))
                         (range num)))
        d1    (first dists)
        d2    (nth dists 1)]
    (cond
      (= (:dist d1) (:dist d2)) -1
      :else                     (:idx d1))))

;; Fill the field with values that indicate which coordinate (from pairs) is
;; closest to each point on the field.
(defn- fill-field [field pairs]
  (let [max-y (count field)
        max-x (count (first field))]
    (loop [x 0, y 0, field field]
      (cond
        (= y max-y) field
        (= x max-x) (recur 0 (inc y) field)
        :else       (recur (inc x) y
                           (assoc-in field [y x] (find-nearest x y pairs)))))))

;; Determine which of the coordinates actually go off into infinity as far as
;; the size of their surrounding field is concerned.
(defn- get-infinite [field]
  (let [max-y  (count field)
        max-x  (count (first field))
        top    (map #(list % 0) (range max-x))
        bottom (map #(list % (dec max-y)) (range max-x))
        left   (map #(list 0 %) (range max-y))
        right  (map #(list (dec max-x) %) (range max-y))
        all    (concat top bottom left right)]
    (conj (set (map #(get-in field (reverse %)) all)) -1)))

;; Find the largest area around a coordinate. Filter out all coordinates that
;; have infinite fields.
(defn- find-largest-area [pairs]
  (let [field    (create-field pairs)
        filled   (fill-field field pairs)
        infinite (get-infinite filled)]
    (last (sort #(compare (:count %1) (:count %2))
                (map #(hash-map :idx (first %) :count (count (last %)))
                     (filter #(not (infinite (first %)))
                             (group-by identity (flatten filled))))))))

;; The contents of "file" are the list of coordinates on the field. Find the
;; largest buffer-area around one of the coordinates.
(defn p01 [file]
  (->> file
       (read-lines)
       (lines-to-pairs)
       (find-largest-area)
       (:count)))

;; Problem 2

;; Total up the distance from the given (x, y) to all the pairs.
(defn- total-dist [x y pairs]
  (let [p1 (list x y)]
    (apply + (map #(manhattan-dist p1 %) pairs))))

;; Fill in the field with all the summed distances for each point.
(defn- fill-distances [field pairs]
  (let [max-y (count field)
        max-x (count (first field))]
    (loop [x 0, y 0, field field]
      (cond
        (= y max-y) field
        (= x max-x) (recur 0 (inc y) field)
        :else       (recur (inc x) y
                           (assoc-in field [y x] (total-dist x y pairs)))))))

;; Find the "safest" area by filling in the complete field and filtering it
;; down to those whose summed distance-value is less than 10000.
(defn- find-safest-area [pairs]
  (let [field  (create-field pairs)
        filled (fill-distances field pairs)]
    (count (filter #(< % 10000) (flatten filled)))))

;; The contents of "file" are the list of coordinates on the field. Find the
;; largest safe-area in the field.
(defn p02 [file]
  (->> file
       (read-lines)
       (lines-to-pairs)
       (find-safest-area)))
