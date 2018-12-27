(ns advent-2018.day17
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

;;; https://adventofcode.com/2018/day/17

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Parse the lines of text. Each line is an axis with a singular value followed
;; by the other axis with a dotted-range value. Store these as pairs of one
;; value and one list, with each in the proper X/Y position.
(defn- parse-lines [lines]
  (loop [[line & lines] lines, data ()]
    (cond
      (nil? line) (reverse data)
      :else
      (let [parse             (re-find #"(.)=(\d+), .=(\d+)[.][.](\d+)"
                                       line)
            [_ axis n1 n2 n3] parse
            n1                (Integer/parseInt n1)
            n2                (Integer/parseInt n2)
            n3                (Integer/parseInt n3)]
        (cond
          (= axis "x") (recur lines (cons (list n1 (list n2 n3)) data))
          :else        (recur lines (cons (list (list n2 n3) n1) data)))))))

;; Create the set that enumerates all the clay. Walks through the data and
;; adds coordinate pairs to the set based on the ranges.
(defn- create-clay [data]
  (loop [[d & ds] data, clay #{}]
    (cond
      (nil? d) clay
      :else
      (let [[x y] d]
        (cond
          (seq? x) (recur ds
                          (reduce (fn [c x']
                                    (conj c [x' y]))
                                  clay (range (first x) (inc (last x)))))
          :else    (recur ds
                          (reduce (fn [c y']
                                    (conj c [x y']))
                                  clay (range (first y) (inc (last y))))))))))

;; These are the "movement" fn's.
(defn- up    [p] (mapv + p [0 -1]))
(defn- down  [p] (mapv + p [0 1]))
(defn- left  [p] (mapv + p [-1 0]))
(defn- right [p] (mapv + p [1 0]))

;; Process the falling water. From a starting point, goes down until it hits
;; a clay coordinate and adds all the in-between points to the flowing set.
(defn- fall [pos lo-y clay flowing]
  (loop [pos pos, flowing flowing]
    (cond
      (>= (last pos) lo-y) (list nil flowing)
      :else
      (let [posd (down pos)]
        (cond
          (clay posd) (list pos flowing)
          :else       (recur posd (conj flowing posd)))))))

;; Handle all the coordinates in the falling set. For each of them, call the
;; fall fn above. At the end, the falling set should be empty, the flowing and
;; spreading sets possibly extended, and the still set unchanged. Both
;; "do-falls" and "do-spreads" have the same return signature for the sake of
;; consistency.
(defn- do-falls [lo-y clay flowing still falling spreading]
  (loop [falling falling, flowing flowing, spreading spreading]
    (let [pt      (first falling)
          falling (disj falling pt)]
      (cond
        (nil? pt) (list flowing still falling spreading)
        :else
        (let [[res flowing] (fall pt lo-y clay flowing)
              spreading     (if res (conj spreading res) spreading)]
          (recur falling flowing spreading))))))

;; Check for spread in a given direction. Does not change the still set, but
;; does extend the temp set.
(defn- spread-dir [pos dir clay still temp]
  (loop [pos' pos, temp temp]
    (cond
      (clay pos') (list nil temp)
      :else
      (let [pos'' (down pos')
            temp  (conj temp pos')]
        (if (or (clay pos'') (still pos''))
          (recur (if (= dir "LEFT") (left pos') (right pos')) temp)
          (list pos' temp))))))

;; Process a spread coordinate. Checks for spread both to the left and to the
;; right, then decides whether to add the "spread" squares to the flowing or
;; still maps.
(defn- spread [pos clay flowing still]
  (let [[sl temp] (spread-dir pos "LEFT" clay still #{})
        [sr temp] (spread-dir pos "RIGHT" clay still temp)]
    (list sl sr
          (if (or sl sr)       (set/union flowing temp) flowing)
          (if (not (or sl sr)) (set/union still temp) still))))

;; Handle all the coordinates in the spreading set. For each, call the spread
;; fn above. At the end, the spreading set should be empty and any of the other
;; three may be modified.
(defn- do-spreads [clay flowing still falling spreading]
  (loop [flowing   flowing
         still     still
         falling   falling
         spreading spreading]
    (let [pt        (first spreading)
          spreading (disj spreading pt)]
      (cond
        (nil? pt) (list flowing still falling spreading)
        :else
        (let [[sl sr flowing still] (spread pt clay flowing still)]
          (if (and (nil? sl)
                   (nil? sr))
            (recur flowing still falling (conj spreading (up pt)))
            (recur flowing still
                   (reduce (fn [f r]
                             (conj f r))
                           falling (filter identity (list sl sr)))
                   spreading)))))))

;; Count the water. If flowing is not passed, count only still water. Union
;; the two sets, as the flowing set will have coordinates in it that have
;; been "upgraded" to still.
(defn- count-of [high still & [flowing]]
  (count (filter #(>= (last %) high) (set/union (or flowing #{}) still))))

;; Run the simulation. Use sets to keep track of all the coordinates of water
;; that is falling, flowing, spreading or still. When there is no more water in
;; either of the falling or spreading sets, the simulation is ended. Based on
;; whether this is part 1 or 2, count all the water or just the still water.
(defn- run-simulation [part data]
  (let [clay (create-clay data)
        lo-y (apply max (map last clay))
        hi-y (apply min (map last clay))]
    (loop [flowing #{}, still #{}, falling #{[500 0]}, spreading #{}]
      (cond
        (empty?
         (set/union falling
                    spreading)) (if (= 1 part)
                                  (count-of hi-y still flowing)
                                  (count-of hi-y still))
        (not-empty falling)     (let [[fl' st'
                                       fa' sp'] (do-falls lo-y clay
                                                          flowing still
                                                          falling spreading)]
                                  (recur fl' st' fa' sp'))
        (not-empty spreading)   (let [[fl' st'
                                       fa' sp'] (do-spreads clay
                                                            flowing still
                                                            falling spreading)]
                                  (recur fl' st' fa' sp'))))))

;; Problem 1

;; Using the survey data in "file", determine all the squares that will have
;; either flowing or still water in them once the flow has gone off the bottom
;; edge of the field.
(defn p01 [file]
  (->> file
       (read-lines)
       (parse-lines)
       (run-simulation 1)))

;; Problem 2

;; Using the survey data in "file", determine all the squares that will have
;; either flowing or still water in them once the flow has gone off the bottom
;; edge of the field. This time, count only the still water, not the flowing.
(defn p02 [file]
  (->> file
       (read-lines)
       (parse-lines)
       (run-simulation 2)))
