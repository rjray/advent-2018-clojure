(ns advent-2018.day21)

;;; https://adventofcode.com/2018/day/21

;; Problem 1

;; Because this actually was based on reverse-engineering the ElfCode input
;; rather than actually running it, there is no input parameter. The steps
;; are based on the bit-operations in the input (which is committed into the
;; repo along with this file, for reference).
(defn p01 []
  (loop [d 3730679, e 0x10000]
    (let [c (bit-and e 0xff)
          d (+ d c)
          d (bit-and d 0xffffff)
          d (* d 65899)
          d (bit-and d 0xffffff)]
      (cond
        (> 256 e) d
        :else     (recur d (quot e 256))))))

;; Problem 2

;; Because this actually was based on reverse-engineering the ElfCode input
;; rather than actually running it, there is no input parameter. For this
;; problem, it was necessary to keep track of values of d that have been seen
;; before, and keep running the main "loop" until a d value is repeated. Of
;; course, this could probably have been written in terms of p01, or the common
;; loop at least extracted out. But, lazy and behind on Advent...
(defn p02 []
  (loop [d 3730679, e 0x10000, s #{}, last-d 0]
    (let [c (bit-and e 0xff)
          d (+ d c)
          d (bit-and d 0xffffff)
          d (* d 65899)
          d (bit-and d 0xffffff)]
      (cond
        (> 256 e) (if (s d)
                    last-d
                    (recur 3730679 (bit-or d 0x10000) (conj s d) d))
        :else     (recur d (quot e 256) s last-d)))))
