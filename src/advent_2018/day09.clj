(ns advent-2018.day09
  (:require [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/9

;; Turn one text-line into a sequence of numbers.
(defn- line-to-numbers [line]
  (map #(Integer/parseInt %) (re-seq #"\d+" line)))

;; Update the score, the circle and the current-marble in the case where a
;; player is scoring.
(defn- update-score [marble circle scores current player]
  (let [target      (loop [i 0, target current]
                      (cond
                        (= i 7) target
                        :else   (recur (inc i) (:prev (circle target)))))
        target-next (:next (circle target))
        target-prev (:prev (circle target))
        s'          (update scores player + marble target)
        c'          (-> circle
                        (assoc-in [target-next :prev] (:prev (circle target)))
                        (assoc-in [target-prev :next] (:next (circle target))))
        cur'        (:next (circle target))]
    (list (conj c' {}) s' cur')))

;; Create a new marble struct.
(defn- new-marble [prev next]
  {:prev prev, :next next})

;; Run the game.
(defn- run-marble-game [[players highval]]
  (let [circle [(new-marble 0 0)]
        scores (vec (repeat players 0))]
    (loop [marble 1, circle circle, scores scores, current 0]
      (cond
        (> marble highval)      (apply max scores)
        (zero? (rem marble 23)) (let [player       (mod marble players)
                                      [c' s' cur'] (update-score marble
                                                                 circle
                                                                 scores
                                                                 current
                                                                 player)]
                                  (recur (inc marble) c' s' cur'))
        :else
        (let [target      (:next (circle current))
              target-next (:next (circle target))
              newmarble   (new-marble target target-next)
              newcircle   (-> circle
                              (assoc-in [target :next] marble)
                              (assoc-in [target-next :prev] marble))]
          (recur (inc marble) (conj newcircle newmarble) scores marble))))))

;; Problem 1

;; The content of "file" is a line describing the number of players and the
;; high-numbered marble. Run the game with these numbers.
(defn p01 [file]
  (as-> file $
    (slurp $)
    (line-to-numbers $)
    (run-marble-game $)))

;; Problem 2

;; The content of "file" is a line describing the number of players and the
;; high-numbered marble. Run the game with the high-number multiplied by 100.
(defn p02 [file]
  (as-> file $
    (slurp $)
    (line-to-numbers $)
    (run-marble-game (list (first $) (* 100 (last $))))))
