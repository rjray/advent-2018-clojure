(ns advent-2018.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/10

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Convert one line of text to a struct that holds both the initial (x,y) of
;; the star and its velocity.
(defn- line-to-struct [line]
  (let [nums (vec (map #(Integer/parseInt %) (re-seq #"-?\d+" line)))]
    {:xpos (nums 0), :ypos (nums 1), :xvel (nums 2), :yvel (nums 3)}))

;; Return a list of all the coordinates described by structs, advanced forward
;; a number of seconds as given in t.
(defn- coords [structs t]
  (doall (map #(list (+ (:xpos %) (* t (:xvel %)))
                     (+ (:ypos %) (* t (:yvel %)))) structs)))

;; Compute the bounding box (and a few other useful values) for the given set
;; of star-coordinates.
(defn- bounding-box [coords]
  (let [xs   (map first coords)
        ys   (map last coords)
        xmin (apply min xs)
        xmax (apply max xs)
        ymin (apply min ys)
        ymax (apply max ys)]
    {:xmin xmin, :xmax xmax, :ymin ymin, :ymax ymax,
     :xoff (- xmin), :yoff (- ymin),
     :width (inc (- xmax xmin)), :height (inc (- ymax ymin))}))

;; Display the given set of star-coordinates, using the parameters in the given
;; bounding-box to govern the size of the display field. Return the value of
;; time at the end, as this is the desired answer for problem 2.
(defn- display-points [coords bbox time]
  (let [{height :height
         width  :width
         xoff   :xoff
         yoff   :yoff} bbox
        field          (vec (repeat height (vec (repeat width " "))))
        field          (reduce (fn [f c]
                                 (assoc-in f (list (+ yoff (last c))
                                                   (+ xoff (first c))) "*"))
                               field coords)]
    (loop [[row & rows] field]
      (cond
        (nil? row) time
        :else      (do (prn (str/join row))
                       (recur rows))))))

;; Run the message by stepping the set of coordinates ahead one second at a
;; time and looking a the change in the size of the bounding box. When a given
;; time t results in a larger box than the previous iteration, the previous
;; iteration was the answer.
(defn- run-message [structs]
  (let [init-coords (coords structs 0)
        init-bbox   (bounding-box init-coords)]
    (loop [t           1
           last-size   (* (:width init-bbox) (:height init-bbox))
           last-coords init-coords
           last-bbox   init-bbox]
      (let [cur-coords (coords structs t)
            cur-bbox   (bounding-box cur-coords)
            cur-size   (* (:width cur-bbox) (:height cur-bbox))]
        (cond
          (> cur-size last-size) (display-points last-coords last-bbox (dec t))
          :else                  (recur (inc t)
                                        cur-size
                                        cur-coords
                                        cur-bbox))))))

;; Problem 1

;; The content of "file" is a series of lines describing the position and
;; velocities of all the stars in the field. Walk them forward a second at
;; a time until the message appears.
(defn p01 [file]
  (->> file
       (read-lines)
       (map line-to-struct)
       (run-message)))

;; For this day, the second problem is just getting the time value from the
;; same run as the first problem. So, no actual difference here.
(defn p02 [file]
  (->> file
       (read-lines)
       (map line-to-struct)
       (run-message)))
