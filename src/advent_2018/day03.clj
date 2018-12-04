(ns advent-2018.day03
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

;;; https://adventofcode.com/2018/day/3

;; The regexp needed to pull out the useful bits of each line:
(def ^:private struct-pattern #"#(\d+)\s+@\s+(\d+),(\d+):\s+(\d+)x(\d+)")

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Turn an input line into a map of (id, left-edge, top-edge, width, height).
(defn- line-to-struct [line]
  (let [m (re-matches struct-pattern line)]
    {:id     (Integer/parseInt (m 1))
     :left   (Integer/parseInt (m 2))
     :top    (Integer/parseInt (m 3))
     :width  (Integer/parseInt (m 4))
     :height (Integer/parseInt (m 5))}))

;; Determine the needed max-size for the field (which is square), based on the
;; list of claims. Minimum is 1000, per the problem definition.
(defn- get-max-size [claims]
  (max (apply max (map #(- (+ (:left %) (:width %)) 1) claims))
       (apply max (map #(- (+ (:top %) (:height %)) 1) claims))
       1000))

;; Create a matrix of size x size using vectors, for faster access.
(defn- create-field [size]
  (vec (repeat size (vec (repeat size [])))))

;; Apply a single claim to the field. Uses conj to update the nested vector, so
;; that we keep track of which specific claims are overlapping on a given
;; point.
(defn- apply-claim [field claim]
  (let [id      (:id claim)
        start-x (:left claim)
        start-y (:top claim)
        max-x   (+ start-x (:width claim))
        max-y   (+ start-y (:height claim))]
    (loop [x start-x, y start-y, field field]
      (cond
        (= y max-y) field
        (= x max-x) (recur start-x (inc y) field)
        :else       (recur (inc x) y (update-in field [y x] conj id))))))

;; Apply the list of claims. Creates a field of NxN, where N is the size we get
;; from looking at the set of claims and measuring the maximum values they
;; describe.
(defn- apply-claims [claims]
  (let [field (create-field (inc (get-max-size claims)))]
    (loop [[claim & claims] claims, field field]
      (cond
        (nil? claim) field
        :else        (recur claims (apply-claim field claim))))))

;;; Problem 1

;; Count the overlaps in a single row, by filtering out those points that have
;; two or more claimants in the nested vector.
(defn- count-overlaps [row]
  (count (filter #(> (count %) 1) row)))

;; Count the total of overlaps by applying count-overlaps to each row in the
;; field.
(defn- total-overlaps [field]
  (apply + (map count-overlaps field)))

;; The data in "file" represents the claims onto the field of cloth. Read it as
;; lines, parse the lines into claim structures, apply them and count the
;; points with multiple claims.
(defn p01 [file]
  (->> file
       (read-lines)
       (map line-to-struct)
       (apply-claims)
       (total-overlaps)))

;;; Problem 2

;; Find the claim that does not overlap with any others. My approach is to put
;; all the IDs into a set, and all IDs that are part of an overlap into a
;; second set. When the whole field has been processed, the "clear" claim is
;; the one that is the result of the difference of the two.
(defn- find-intact-claim [field]
  (let [max-x (count (first field))
        max-y (count field)]
    (loop [x 0, y 0, all #{}, overlap #{}]
      (cond
        (= y max-y) (first (set/difference all overlap))
        (= x max-x) (recur 0 (inc y) all overlap)
        :else       (let [elt (get-in field [y x])]
                      (recur (inc x) y
                             (set/union all (set elt))
                             (if (> (count elt) 1)
                               (set/union overlap (set elt))
                               overlap)))))))

;; The data in "file" represents the claims onto the field of cloth. Read it as
;; lines, parse the lines into claim structures, apply them and determine the
;; one claim that has no overlap with any others.
(defn p02 [file]
  (->> file
       (read-lines)
       (map line-to-struct)
       (apply-claims)
       (find-intact-claim)))
