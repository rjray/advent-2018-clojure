(ns advent-2018.day05
  (:require [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/5

;; "Reduce" a polymer string by removing any adjacent pairs where the letters
;; are the same but the case is different (i.e., "aA", "Dd", etc.). Return a
;; string with the reduced polymer.
(defn- reduce-polymer [p]
  (loop [[c & p] p, res ()]
    (let [r (first res)]
      (cond
        (nil? c)                     (apply str (reverse res))
        (nil? r)                     (recur p (cons c res))
        (and (not= c r)
             (= (str/upper-case c)
                (str/upper-case r))) (recur p (rest res))
        :else                        (recur p (cons c res))))))

;;; Problem 1

;; The data in "file" is the polymer string, a long string of upper and lower
;; case letter. Apply the "reduction" algorithm given in the problem
;; description.
(defn p01 [file]
  (->> file
       (slurp)
       (str/trim-newline)
       (reduce-polymer)
       (count)))

;;; Problem 2

;; Drop all units in the string "p" that match either the upper-case or
;; lower-case character indicated by "code". The value of code is the integer
;; ASCII value for the upper-case.
(defn- drop-units [p code]
  (let [uc (char code), lc (char (+ code 32))]
    (filter #(not= lc %) (filter #(not= uc %) p))))

;; Get the reduced version of the polymer "p" after all units identified
;; by "code" have been removed.
(defn- get-reduced [p c]
  (let [trimmed (drop-units p c)
        p'      (reduce-polymer trimmed)]
    {:string p', :length (count p')}))

;; Find the shorted reduced polymer across the range of all 26 letters being
;; possibly removed before reduction.
(defn- find-shortest [p]
  (:string (first (sort #(compare (:length %1) (:length %2))
                        (map #(get-reduced p %) (range 65 91))))))

;; The data in "file" is the polymer string, a long string of upper and lower
;; case letter. Apply the "reduction" algorithm given in the problem
;; description. For this case, also remove potentially "troublesome" units and
;; report the length of the optimal string.
(defn p02 [file]
  (->> file
       (slurp)
       (str/trim-newline)
       (find-shortest)
       (count)))
