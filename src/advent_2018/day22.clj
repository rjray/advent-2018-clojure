(ns advent-2018.day22
  (:require [advent-2018.heap :as h]))

;;; https://adventofcode.com/2018/day/22

;; Calculate the geological index for (x,y) in the table. The (tx,ty) are
;; passed because the target's square has a geo index of 0.
(defn- geo-index [x y tx ty table]
  (cond
    (and (zero? x)
         (zero? y)) 0
    (and (= x tx)
         (= y ty))  0
    (zero? x)       (* y 48271)
    (zero? y)       (* x 16807)
    :else
    (let [el-up   (get-in table [(dec y) x])
          el-left (get-in table [y (dec x)])]
      (* el-up el-left))))

;; Calculate the erosion level from the geo index value and the depth.
(defn- erosion-level [geo-idx depth]
  (mod (+ geo-idx depth) 20183))

;; Create the cave matrix. Create it from (0,0) to (tx,ty), unless the
;; "extra" param is given. In that case, extend the cave by that amount in both
;; directions.
(defn- create-cave-struct [depth tx ty & [extra]]
  (let [extra (or extra 0)
        dx    (+ tx extra)
        dy    (+ ty extra)
        cave  (vec (repeat (inc ty) (vec (repeat (inc tx) nil))))]
    (reduce (fn [c coords]
              (let [geo (geo-index (first coords) (last coords) tx ty c)]
                (assoc-in c (reverse coords) (erosion-level geo depth))))
            cave (for [y (range (inc dy)), x (range (inc dx))] [x y]))))

;; Problem 1

;; Sum up the risk values for all squares from the mouth (0,0) to the target
;; (tx,ty).
(defn- calculate-risk [[depth tx ty]]
  (let [cave (create-cave-struct depth tx ty)]
    (apply + (for [y (range (inc ty)), x (range (inc tx))]
               (mod (get-in cave [y x]) 3)))))

;; The file tells the deptth of the cave area, and the location of the target.
;; Use these to create the cave and then calculate the total risk for the box
;; area that extends from the mount (0,0) to the target.
(defn p01 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (calculate-risk)))

;; Problem 2

;; Calculate the risk at square (x,y). Could have used this on p01 too, I
;; suppose...
(defn- risk [x y cave]
  (mod (get-in cave [y x]) 3))

;; Generate the new search nodes related to costs of changing equipment in
;; a given room.
(defn- room-costs [minutes x y cannot cave]
  (for [i (range 3)
        :when (and (not= i cannot)
                   (not= i (risk x y cave)))]
    {:key [(+ 7 minutes) x y i]}))

;; Generate the new search nodes related to moving in the four allowable
;; directions.
(defn- moves [minutes x y cannot cave]
  (let [dirs (list [-1, 0] [1, 0] [0, -1] [0, 1])]
    (loop [[dir & dirs] dirs, moves ()]
      (cond
        (nil? dir) moves
        :else
        (let [[dx dy] dir, newx (+ x dx), newy (+ y dy)]
          (if (and (>= newx 0)
                   (>= newy 0)
                   (not= cannot (risk newx newy cave)))
            (recur dirs (cons {:key  [(inc minutes)
                                      newx newy cannot]} moves))
            (recur dirs moves)))))))

;; Find the shorted path-time from the mount (0,0) to the target. This is a
;; modified Dijkstra algorithm that uses a min-heap/priority queue structure to
;; keep the nodes in the frontier properly sorted. To do this, I am re-using
;; the heap code written earlier this year for the Coursera Algorithms
;; Specialization. With some modifications (that raelly should have been in the
;; original anyway).
(defn- find-path-time [[depth tx ty]]
  (let [cave   (create-cave-struct depth tx ty 1000)
        heap   (h/min-heap (list {:key [0 0 0 1]}))
        target [tx ty 1]]
    (loop [q heap, best {}]
      (cond
        (zero? (h/size q)) "failed"
        :else
        (let [{[minutes x y cannot] :key} (h/root q)
              best-key             [x y cannot]
              q'                   (h/delete q)]
          (cond
            (and (contains? best best-key)
                 (<= (best best-key) minutes)) (recur q' best)
            (= best-key target)                minutes
            :else
            (recur (reduce h/insert q'
                           (concat (room-costs minutes x y cannot cave)
                                   (moves minutes x y cannot cave)))
                   (assoc best best-key minutes))))))))

;; This time, actually try to find a shortest-path (in terms of time) to the
;; target.
(defn p02 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (find-path-time)))
