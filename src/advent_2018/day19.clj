(ns advent-2018.day19
  (:require [clojure.java.io :as io]))

;;; https://adventofcode.com/2018/day/19

;;; The opcodes for the "machine". Copied from day16.clj.

(defn- op-addr [[_ A B C] rs] (assoc rs C (+ (rs A) (rs B))))
(defn- op-addi [[_ A B C] rs] (assoc rs C (+ (rs A) B)))
(defn- op-mulr [[_ A B C] rs] (assoc rs C (* (rs A) (rs B))))
(defn- op-muli [[_ A B C] rs] (assoc rs C (* (rs A) B)))
(defn- op-banr [[_ A B C] rs] (assoc rs C (bit-and (rs A) (rs B))))
(defn- op-bani [[_ A B C] rs] (assoc rs C (bit-and (rs A) B)))
(defn- op-borr [[_ A B C] rs] (assoc rs C (bit-or (rs A) (rs B))))
(defn- op-bori [[_ A B C] rs] (assoc rs C (bit-or (rs A) B)))
(defn- op-setr [[_ A B C] rs] (assoc rs C (rs A)))
(defn- op-seti [[_ A B C] rs] (assoc rs C A))
(defn- op-gtir [[_ A B C] rs] (assoc rs C (if (> A (rs B)) 1 0)))
(defn- op-gtri [[_ A B C] rs] (assoc rs C (if (> (rs A) B) 1 0)))
(defn- op-gtrr [[_ A B C] rs] (assoc rs C (if (> (rs A) (rs B)) 1 0)))
(defn- op-eqir [[_ A B C] rs] (assoc rs C (if (= A (rs B)) 1 0)))
(defn- op-eqri [[_ A B C] rs] (assoc rs C (if (= (rs A) B) 1 0)))
(defn- op-eqrr [[_ A B C] rs] (assoc rs C (if (= (rs A) (rs B)) 1 0)))

;; A map of opcode names to the above functions.
(def ^:private opcodes {"addr" op-addr
                        "addi" op-addi
                        "mulr" op-mulr
                        "muli" op-muli
                        "banr" op-banr
                        "bani" op-bani
                        "borr" op-borr
                        "bori" op-bori
                        "setr" op-setr
                        "seti" op-seti
                        "gtir" op-gtir
                        "gtri" op-gtri
                        "gtrr" op-gtrr
                        "eqir" op-eqir
                        "eqri" op-eqri
                        "eqrr" op-eqrr})

;;; End of opcodes-data from day16.clj.

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Parse the input. Set up the series of instructions as a vector so that they
;; can be easily referenced by index. Add the ip value as an additional element
;; at the end of the instruction vector. Any "#ip n" lines will change
;; the "current value" of this.
(def ^:private line-re #"(\w{4}|#ip)\s+(\d+)(?:\s+(\d+)\s+(\d+))?")
(defn- parse-input [lines]
  (loop [[line & lines] lines, ip 0, program []]
    (cond
      (nil? line) program
      :else
      (let [[inst & nums] (rest (re-find line-re line))
            nums          (map #(if-not (nil? %) (Integer/parseInt %)) nums)]
        (cond
          (= inst "#ip") (recur lines (first nums) program)
          :else
          (recur lines ip
                 (conj program
                       (conj (vec (cons (opcodes inst) nums)) ip))))))))

;; Optimization of the loop in lines 3-11 of the "program". Without this, p02
;; is prohibitively long to run.
(defn- ip-3-loop [_ rs ipr]
  (cond
    (zero? (rem (rs 5) (rs 1))) (assoc rs 2 1, 3 (rs 5), ipr 6)
    :else                       (assoc rs 2 1, 3 (inc (rs 5)), ipr 11)))

;; Run the "program" given the starting registers values.
(defn- run [starting-registers program]
  (let [maxmem (count program)]
    (loop [ipr (last (first program)), registers starting-registers]
      (let [ip (registers ipr)]
        (cond
          (>= ip maxmem) registers
          :else
          (let [command (program ip)
                result  (if (= ip 3)
                          (ip-3-loop command registers ipr)
                          ((first command) command registers))
                newip   (result ipr)]
            ;;(printf "ip=%2d %s %s %s\n" ip registers command result)
            (recur (last command) (assoc result ipr (inc newip)))))))))

;; Problem 1

;; Run the program in "file", with a set of all-zeros in the registers.
(defn p01 [file]
  (->> file
       (read-lines)
       (parse-input)
       (run [0 0 0 0 0 0])))

;; Problem 2

;; Run the program again, only this time with register 0 starting at 1
;; rather than 0.
(defn p02 [file]
  (->> file
       (read-lines)
       (parse-input)
       (run [1 0 0 0 0 0])))
