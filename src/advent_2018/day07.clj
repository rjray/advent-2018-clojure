(ns advent-2018.day07
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

;;; https://adventofcode.com/2018/day/7

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Parse out the two steps from the line of text and create a list.
(defn- line-to-pair [line]
  (rest (re-find #"Step (\w) must be finished before step (\w)" line)))

;; Create the pairs for all the lines.
(defn- lines-to-pairs [lines]
  (map line-to-pair lines))

;; Set up the initial graph by determining all the nodes that will be in it,
;; then initializing each label to an empty set.
(defn- setup-graph [pairs]
  (reduce (fn [graph key]
            (assoc graph key #{}))
          {} (distinct (flatten pairs))))

;; Create the graph of steps with their dependencies.
(defn- create-graph [pairs]
  (reduce (fn [ret [dep key]]
            (update ret key conj dep))
          (setup-graph pairs) pairs))

;; Remove the given step from the graph. This means both removing it's node and
;; also removing it from the dependency sets of any nodes that had it as a
;; dependency.
(defn- remove-step [graph step]
  (let [graph' (dissoc graph step)]
    (reduce (fn [ret key]
              (update ret key disj step))
            graph' (keys graph'))))

;; Problem 1

;; Find the next step to be executed. This will be the step that has zero
;; dependencies. If more than one has zero, then they are taken in
;; alphabetical order.
(defn- find-next-step [graph]
  (let [sortable (sort #(compare (first %1) (first %2))
                       (map #(list (first %) (count (last %))) graph))]
    (ffirst (sort #(compare (last %1) (last %2)) sortable))))

;; Find the order in which the steps have to be done, based on their sets
;; of dependencies.
(defn- find-step-order [graph]
  (loop [g graph, steps ()]
    (cond
      (empty? g) (apply str (reverse steps))
      :else
      (let [step (find-next-step g)]
        (recur (remove-step g step) (cons step steps))))))

;; The content of "file" is the list of step-dependency statements for the
;; graph. We want to find the sequence in which the steps have to be done.
(defn p01 [file]
  (->> file
       (read-lines)
       (lines-to-pairs)
       (create-graph)
       (find-step-order)))

;; Problem 2

;; Return a (possibly) new value for the passed-in worker value. If the
;; work is currently not idle, mark them idle if their step is now
;; complete.
(defn- clear-if-complete [worker seconds]
  (cond
    (nil? worker) worker
    :else         (when-not (= seconds (last worker)) worker)))

;; Return a structure of a new workers vector, new graph, and new set of
;; currently-running steps. This is based on the removal of completed steps
;; from the workers vector and removing those steps from the graph and the
;; set of currently-running.
(defn- remove-complete [workers graph running seconds]
  (let [done (filter #(and (not (nil? %)) (= seconds (last %))) workers)]
    {:workers (vec (map #(clear-if-complete % seconds) workers))
     :graph   (reduce (fn [ret x]
                        (remove-step graph (first x)))
                      graph done)
     :running (apply disj running (map first done))}))

;; Create a worker job from the given step. The time for the step is the base
;; value plus the ordinal number of the step's letter. This is added to the
;; current seconds to determine when the step will be completed.
(defn- create-job [step base secs]
  (list (first step)
        (+ secs base (- (int (ffirst step)) 64))))

;; Return a structure of a new workers vector and new set of currently-running
;; steps. This is based on scheduling any idle workers with any ready-to-go
;; steps.
(defn- schedule-workers [workers graph running base secs]
  (let [wc    (count workers)
        steps (map #(list (first %) (count (last %))) graph)
        steps (filter #(zero? (last %)) steps)
        steps (filter #(not (running (first %))) steps)
        steps (sort #(compare (first %1) (first %2)) steps)]
    (loop [workers workers, idx 0, steps steps, running running]
      (cond
        (or (= idx wc)
            (empty? steps)) {:workers workers, :running running}
        :else
        (let [worker (workers idx)]
          (cond
            (nil? worker) (recur (assoc workers
                                        idx
                                        (create-job (first steps) base secs))
                                 (inc idx)
                                 (rest steps)
                                 (conj running (ffirst steps)))
            :else         (recur workers (inc idx) steps running)))))))

;; A boolean predicate to see if all the workers in the "workers" vector
;; are currently idle.
(defn- all-idle? [workers]
  (zero? (count (remove nil? workers))))

;; Find the total time it takes to do all the steps in the graph, based on
;; the number of workers available and the base-time needed for each step.
(defn- find-total-time [nworkers basetime graph]
  (loop [seconds 0
         workers (vec (repeat nworkers nil))
         graph   graph
         running #{}]
    (let [{w' :workers
           g' :graph
           r' :running} (remove-complete workers graph running seconds)
          {w' :workers
           r' :running} (schedule-workers w' g' r' basetime seconds)]
      (cond
        (all-idle? w') seconds
        :else          (recur (inc seconds) w' g' r')))))

;; The content of "file" is the list of step-dependency statements for the
;; graph. We want to find out how many seconds it will take to do the
;; steps, given 5 parallel workers and a base time-cost of 60s for each
;; given step.
(defn p02 [file]
  (->> file
       (read-lines)
       (lines-to-pairs)
       (create-graph)
       (find-total-time 5 60)))
