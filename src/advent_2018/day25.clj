(ns advent-2018.day25
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [abs]]))

;;; https://adventofcode.com/2018/day/25

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Convert a single line to a vector of 4 coordinate values.
(defn- line-to-coords [line]
  (mapv #(Integer/parseInt %) (re-seq #"-?\d+" line)))

;; Convert the lines to vectors of 4-tuple coordinates.
(defn- lines-to-coords [lines]
  (map line-to-coords lines))

;; Calculate the Manhattan Distance between two points:
(defn- manhattan-dist [p1 p2]
  (apply + (map #(abs (- %1 %2)) p1 p2)))

;; Create the basic struct for doing the Union-Find algorithm.
(defn- union-find [n]
  {:n n, :parents (vec (repeat n nil)), :ranks (vec (repeat n 1)), :num-sets n})

;; Find the parent of node i in the UF structure. As this may alter the
;; structure, the return value is the updated struct followed by the
;; find-value.
(defn- uf-find [uf i]
  (let [p ((:parents uf) i)]
    (if (nil? p)
      (list uf i)
      (let [[uf p] (uf-find uf p)
            uf     (assoc-in uf [:parents i] p)]
        (list uf p)))))

;; Merge two sets into one. Returns the updated UF structure.
(defn- uf-merge [uf i j]
  (let [[uf i] (uf-find uf i)
        [uf j] (uf-find uf j)]
    (if (= i j)
      uf
      (let [i-rank (get-in uf [:ranks i])
            j-rank (get-in uf [:ranks j])
            uf     (cond
                     (< i-rank j-rank) (assoc-in uf [:parents i] j)
                     (> i-rank j-rank) (assoc-in uf [:parents j] i)
                     :else             (update-in (assoc-in uf [:parents j] i)
                                                  [:ranks i] inc))]
        (update uf :num-sets dec)))))

;; Merge a point into an existing set in the UF structure.
(defn- merge-point [i p pt-map uf]
  (loop [[pt & pts] (keys pt-map), uf uf]
    (cond
      (nil? pt)                    uf
      (<= (manhattan-dist p pt) 3) (recur pts (uf-merge uf i (pt-map pt)))
      :else                        (recur pts uf))))

;; Cluster the points by comparing them in the Union-Find structure one at a
;; time, using merge-point above. It will merge the two sets that the new point
;; and the previous points represent, based on whether the Manhattan Distance
;; is small enough.
(defn- cluster-points [points]
  (let [uf (union-find (count points))]
    (loop [[p & points] points, i 0, pt-map {}, uf uf]
      (cond
        (nil? p) uf
        :else    (recur points (inc i) (assoc pt-map p i)
                        (merge-point i p pt-map uf))))))

;; Problem 1

;; Read the set of 4-dimensional points in the given file. Run the clustering
;; algorithm on them, then report the resulting number of sets.
(defn p01 [file]
  (->> file
       (read-lines)
       (lines-to-coords)
       (cluster-points)
       (:num-sets)))

;; There is no problem 2
