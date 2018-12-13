(ns advent-2018.day12
  (:require [clojure.java.io :as io]))

;;; https://adventofcode.com/2018/day/12

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Create the rules-map from the rules lines. We do this by converting the
;; five-character string into a 5-element seq of chars and use that as the key.
;; The value at each key is the second matched part of the line.
(defn- create-rules [rules]
  (reduce (fn [m [pat val]]
            (assoc m (seq pat) (first val)))
          {} (map #(re-seq #"[#.]+" %) rules)))

;; Parse the set of lines that were read in, into a structure that describes
;; the state of the plants, the current offset into the state for pot 0, and
;; the set of transition rules.
(defn- parse-input [lines]
  (let [[initial _ & rules] lines
        initial-state       (first (re-find #"([#.]+)" initial))]
    {:state  (vec initial-state)
     :offset 0
     :rules  (create-rules rules)}))

;; Create a 5-element seq of the state of the pots centered on "i". This will
;; be used to look up the transistion rule for the pot at i.
(defn- pots-at [state i max]
  (map #(cond
          (or (neg? %)
              (>= % max)) \.
          :else           (state %))
       (range (- i 2) (+ i 3))))

;; Generate a vector of 0-2 elements, depending on whether any new slots are
;; needed on the left side of the state to accomate new plants.
(defn- get-new-left [state rules max]
  (let [cell1 (get rules (pots-at state -2 max) \.)
        cell2 (get rules (pots-at state -1 max) \.)]
    (cond
      (= cell1 \#) [cell1 cell2]
      (= cell2 \#) [cell2]
      :else        [])))

;; Generate a vector of 0-2 elements, depending on whether any new slots are
;; needed on the right side of the state to accomate new plants.
(defn- get-new-right [state rules max]
  (let [cell1 (get rules (pots-at state max max) \.)
        cell2 (get rules (pots-at state (inc max) max) \.)]
    (cond
      (= cell2 \#) [cell1 cell2]
      (= cell1 \#) [cell1]
      :else        [])))

;; "Mutate" the given structure by returning a new structure with any needed
;; changes implemented. The :rules key won't change, but :state definitely will
;; and :offset might.
(defn- mutate [struct]
  (let [{state  :state
         offset :offset
         rules  :rules} struct
        max             (count state)
        new-left        (get-new-left state rules max)
        new-right       (get-new-right state rules max)
        new-center      (reduce (fn [v i]
                                  (conj v (get rules (pots-at state i max) \.)))
                                [] (range max))]
    {:state  (vec (concat new-left new-center new-right))
     :offset (+ offset (count new-left))
     :rules  rules}))

;; Calculate the score of the given state vector with pot 0 located at "offset"
;; within the vector.
(defn- score [state offset]
  (apply + (map #(if (= (state %) \#) (- % offset) 0) (range (count state)))))

;; Mutate the given struct through "generations" iterations of the
;; transformation rules. To avoid excessive computation, this will check for
;; the deltas between generations to stabilize. If that happens, project the
;; final answer by scaling the stable delta over the remaining generations.
(defn- run-generations [generations struct]
  (let [{state  :state
         offset :offset
         rules  :rules} struct
        init-score      (compute-score state offset)]
    (loop [gen 0, struct struct, last-score init-score, deltas (list 0)]
      (cond
        (= gen generations) (score (:state struct) (:offset struct))
        :else
        (let [new-struct (mutate struct)
              new-score  (score (:state new-struct) (:offset new-struct))
              new-deltas (cons (- new-score last-score) deltas)]
          (cond
            (and (> (count deltas) 5)
                 (apply = (take 5 deltas))) (+ new-score
                                               (* (first new-deltas)
                                                  (dec (- generations gen))))
            :else
            (recur (inc gen) new-struct new-score new-deltas)))))))

;; Problem 1

;; The contents of "file" are the initial state, a blank line, and the rules
;; for state transitions. Run the data for 20 generations.
(defn p01 [file]
  (->> file
       (read-lines)
       (parse-input)
       (run-generations 20)))

;; The contents of "file" are the initial state, a blank line, and the rules
;; for state transitions. Run the data for 50000000000 generations.
(defn p02 [file]
  (->> file
       (read-lines)
       (parse-input)
       (run-generations 50000000000)))
