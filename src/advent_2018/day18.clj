(ns advent-2018.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/18

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Convert the lines into the NxN matrix of characters.
(defn- create-field [lines]
  (mapv #(vec (seq %)) (map str/trim-newline lines)))

;; Get the values in the squares adjacent to the given one.
(defn- get-adjacent [field x y max-x max-y]
  (for [tx (range (dec x) (+ x 2))
        ty (range (dec y) (+ y 2))
        :when (and (>= tx 0)
                   (>= ty 0)
                   (< tx max-x)
                   (< ty max-y)
                   (not= [x y] [tx ty]))]
    (get-in field [ty tx])))

;; Tally the counts of trees and yards in the seq that came from above.
(defn- tally [field x y max-x max-y]
  (let [resources (get-adjacent field x y max-x max-y)]
    {:trees (count (filter #(= % \|) resources))
     :yards (count (filter #(= % \#) resources))}))

;; Advance the field one iteration.
(defn- advance [field]
  (let [max-y (count field)
        max-x (count (first field))]
    (loop [x 0, y 0, out field]
      (cond
        (= y max-y) out
        (= x max-x) (recur 0 (inc y) out)
        :else
        (let [surrounding (tally field x y max-x max-y)
              cur         (get-in field [y x])
              new         (cond
                            (and (= cur \.)
                                 (>= (:trees surrounding) 3))       \|
                            (and (= cur \|)
                                 (>= (:yards surrounding) 3))       \#
                            (and (= cur \#)
                                 (or (zero? (:trees surrounding))
                                     (zero? (:yards surrounding)))) \.
                            :else                                   cur)]
          (recur (inc x) y (assoc-in out [y x] new)))))))

;; Look for a cycle by trying to match the value of field (the current state of
;; the field) to any of the ones in the history. If we find a match, return the
;; structure with an extra field giving the period of the cycle.
(defn- find-cycle [field iter hist]
  (let [match (first (filter #(= field (:field %)) hist))]
    (if-not (nil? match) (assoc match :period (- iter (:iter match))))))

;; Using a match detected by find-cycle and enough of the history, return the
;; field that would actually occur on the last iteration.
(defn- extrapolate [match iter n hist]
  (let [index  (mod (- n iter) (:period match))
        choice (nth (reverse hist) index)]
    (:field choice)))

;; Run the simulation for n generations, using the initial field. Look for
;; possible cycles to avoid iterating over all of n.
(defn- run-generations [n field]
  (loop [iter 0, field field, history ()]
    (cond
      (= iter n)     field
      :else
      (let [match (find-cycle field iter history)]
        (cond
          match (extrapolate match iter n (take (:period match) history))
          :else
          (recur (inc iter)
                 (advance field)
                 (take 100 (cons {:iter iter, :field field} history))))))))

;; Calculate the resource value for the given field.
(defn- calc-resource-value [field]
  (let [all   (flatten field)
        trees (count (filter #(= % \|) all))
        yards (count (filter #(= % \#) all))]
    (* trees yards)))

;; Problem 1

;; Read the input data in "file", create the initial field and run 10
;; generations. Return the resource value at the last generation.
(defn p01 [file]
  (->> file
       (read-lines)
       (create-field)
       (run-generations 10)
       (calc-resource-value)))

;; Problem 2

;; Run the same simulation, but for 1000000000 iterations. We actually
;; detect any cycle and use that to extrapolate the answer rather than
;; actually running that many iterations.
(defn p02 [file]
  (->> file
       (read-lines)
       (create-field)
       (run-generations 1000000000)
       (calc-resource-value)))
