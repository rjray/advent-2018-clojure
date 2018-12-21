(ns advent-2018.day20)

;;; https://adventofcode.com/2018/day/20

;; "Coerce" a boolean into a numerical value of 0 or 1 (false/true).
(defn- coerce [bool] (if bool 1 0))

;; Build a grid structure with distances from the center point. We'll use
;; this on both problems by looking at the distances that result.
(defn- build-grid [regexp]
  (loop [[ch & chars] regexp, grid {[0 0] 0}, dist 0, x 0, y 0, stack ()]
    (cond
      (nil? ch) grid
      ;; Skip ^ and $ for this algorithm
      (= ch \^) (recur chars grid dist x y stack)
      (= ch \$) (recur chars grid dist x y stack)
      (= ch \() (recur chars grid dist x y (cons [dist x y] stack))
      (= ch \)) (let [top (first stack)]
                  (recur chars grid (top 0) (top 1) (top 2) (rest stack)))
      (= ch \|) (let [top (first stack)]
                  (recur chars grid (top 0) (top 1) (top 2) stack))
      :else
      (let [x    (+ x (- (coerce (= ch \E)) (coerce (= ch \W))))
            y    (+ y (- (coerce (= ch \S)) (coerce (= ch \N))))
            dist (inc dist)
            grid (if (< dist (get grid [x y] Integer/MAX_VALUE))
                   (assoc grid [x y] dist)
                   grid)]
        (recur chars grid dist x y stack)))))

;; Problem 1

;; Find the value of the room that is the most doors away from the origin.
(defn- most-doors [grid]
  (apply max (vals grid)))

;; The content of "file" is a regular expression that describes the building's
;; room layout. Find the longest-distance-away room (in term of doors walked
;; through) from the origin.
(defn p01 [file]
  (->> file
       (slurp)
       (clojure.string/trim-newline)
       (build-grid)
       (most-doors)))

;; Problem 2

;; Count the number of values that are equal to or greater than n.
(defn- count-dists-above-equal [n grid]
  (count (filter #(>= % n) (vals grid))))

;; The content of "file" is a regular expression that describes the building's
;; room layout. Find the number of rooms that are 1000 doors or more away from
;; the origin.
(defn p02 [file]
  (->> file
       (slurp)
       (clojure.string/trim-newline)
       (build-grid)
       (count-dists-above-equal 1000)))
