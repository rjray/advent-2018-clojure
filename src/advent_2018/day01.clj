(ns advent-2018.day01)

;;; https://adventofcode.com/2018/day/1

;;; Problem 1

;; Each line of text in "file" represents a frequency change (positive or
;; negative). Starting from 0, calculate the final frequency obtained by
;; applying all the changes in order.
(defn p01 [file]
  (->> file
       (slurp)
       (re-seq #"[+-]\d+")
       (map #(Integer/parseInt %))
       (reduce +)))

;;; Problem 2

;; From an input stream "is", which is infinite, find the first frequency value
;; that appears twice. The frequency sequence starts at 0.
(defn- find-dup-freq [is]
  (loop [acc 0, seen #{0}, [freq & freqs] is]
    (let [newacc (+ acc freq)]
      (cond
        (seen newacc) newacc
        :else         (recur newacc (conj seen newacc) freqs)))))

;; Each line of text in "file" represents a frequency change (positive or
;; negative). Starting from 0, calculate the frequency at each point from
;; adding or subtracting the change value. Repeat the sequence of frequency
;; changes until you see a frequency value at a step that has been seen before.
;; Return this duplicate value.
(defn p02 [file]
  (->> file
       (slurp)
       (re-seq #"[+-]\d+")
       (map #(Integer/parseInt %))
       (cycle)
       (find-dup-freq)))
