(ns advent-2018.day02
  (:require [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/2

;;; Problem 1

;; Given a map made from an ID, in which each key points to a vector of the
;; same char repeated as many times as it appeared in the ID, return true or
;; false based on whether the map has one or more of these vectors with exactly
;; n items.
(defn- has-n-occurrences? [n id-map]
  (->> id-map
       (vals)
       (filter #(= n (count %)))
       (count)
       (pos?)))

;; Given a sequence of maps in which a character key points to a vector of that
;; same character in which the number of elements is how often that character
;; appears in the ID the map was derived from. Return a count of how many of
;; these have at least one character that appears *exactly* n times.
(defn- count-n-occurrences [n maps]
  (count (filter #(has-n-occurrences? n %) maps)))


;; Each line of "file" is a 26-character ID string. Make two counts over the
;; list: those that contain at least one letter that appears exactly twice, and
;; those that contain at least one letter that appears exactly three times. A
;; given ID may count for both of those. Produce a "checksum" that is the
;; product of those two numbers.
(defn p01 [file]
  (as-> file $
    (slurp $)
    (re-seq #"[a-z]+" $)
    (map #(group-by identity %) $)
    (* (count-n-occurrences 2 $) (count-n-occurrences 3 $))))

;;; Problem 2

;; Do the two given IDs differ by exactly one character in the same place?
(defn- differ-by-one? [[id1 id2]]
  (= 1 (count (filter false? (map = id1 id2)))))

;; Given two IDs, return the string made up of all letters that match in the
;; same position.
(defn- matching-letters [[id1 id2]]
  (str/join (map #(if (= %1 %2) %1 "") id1 id2)))

;; Each line of "file" is a 26-character ID string. Find the pair in which the
;; IDs differ by exactly one character (in the same place). Return the string
;; of common letters that result from removing the mismatch letters.
(defn p02 [file]
  (as-> file $
    (slurp $)
    (re-seq #"[a-z]+" $)
    (comb/combinations $ 2)
    (filter differ-by-one? $)
    (first $)
    (matching-letters $)))
