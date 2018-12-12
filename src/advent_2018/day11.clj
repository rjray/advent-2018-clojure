(ns advent-2018.day11)

;;; https://adventofcode.com/2018/day/11
;;; Ref: https://en.wikipedia.org/wiki/Summed-area_table

;; Calculate the power value for the cell at (x,y), based on the grid S/N.
(defn- power [x y gsn]
  (let [rackid (+ x 10)]
    (-> rackid
        (* y)
        (+ gsn)
        (* rackid)
        (rem 1000)
        (/ 100)
        (int)
        (- 5))))

;; Build a 301x301 matrix of zeroes.
(defn- build-grid [gsn]
  (vec (repeat 301 (vec (repeat 301 0)))))

;; Populate the grid with the 2D partial sums over the table (in the ranges
;; (1-300,1-300)).
(defn- build-sums [gsn grid]
  (reduce (fn [g cs]
            (let [[y x] cs]
              (assoc-in g cs (+ (power x y gsn)
                                (get-in g [(dec y) x])
                                (get-in g [y (dec x)])
                                (- (get-in g [(dec y) (dec x)]))))))
          grid (for [y (range 1 301), x (range 1 301)] [y x])))

;; Find the best square-sum within the grid. If "size" is provided, look only
;; at sub-grids of that size. Otherwise, try all sizes from 1 to 300.
(defn- get-best [grid & [size]]
  (let [start (or size 1)
        end   (or size 300)
        best  {:x 0, :y 0, :s 0, :sum (Integer/MIN_VALUE)}]
    (loop [s start, y s, x s, best best]
      (cond
        (> s end) best
        (= y 301) (recur (inc s) (inc s) (inc s) best)
        (= x 301) (recur s (inc y) s best)
        :else     (let [total (- (get-in grid [y x])
                                 (get-in grid [(- y s) x])
                                 (get-in grid [y (- x s)])
                                 (- (get-in grid [(- y s) (- x s)])))]
                    (cond
                      (> total (:sum best)) (recur s y (inc x) {:x x
                                                                :y y
                                                                :s s
                                                                :sum total})
                      :else                 (recur s y (inc x) best)))))))

;; Problem 1

;; For problem 1, we only need the (x,y) coordinates.
(defn- format-answer-1 [ans]
  (format "%d,%d"
          (inc (- (:x ans) (:s ans)))
          (inc (- (:y ans) (:s ans)))))

;; Get the best sub-grid sum for sub-grids of 3x3 only.
(defn p01 [serial]
  (as-> serial $
    (build-grid $)
    (build-sums serial $)
    (get-best $ 3)
    (format-answer-1 $)))

;; Problem 2

;; For problem 2, we need the (x,y) coordinates and the size of the sub-grid.
(defn- format-answer-2 [ans]
  (format "%d,%d,%d"
          (inc (- (:x ans) (:s ans)))
          (inc (- (:y ans) (:s ans)))
          (:s ans)))

;; Get the best sub-grid sum for any arbitrarily-sized sub-grid.
(defn p02 [serial]
  (as-> serial $
    (build-grid $)
    (build-sums serial $)
    (get-best $)
    (format-answer-2 $)))
