(ns advent-2018.day16
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/16

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Parse the lines into forms usable for the puzzle problems. Because the input
;; is essentially two separate things, use a pseudo-state machine to know when
;; to stop populating the samples and start populating the program.
(defn- parse-input [lines]
  (loop [[l & lines]  lines
         samples   []
         program   []
         last-seen :blank]
    (cond
      (nil? l) {:samples (partition 3 samples), :program program}
      :else
      (let [nums (vec (map #(Integer/parseInt %) (re-seq #"\d+" l)))]
        (cond
          ;; Skip blank lines
          (empty? (str/trim-newline l)) (recur lines samples program :blank)
          (str/starts-with? l "Before") (recur lines (conj samples nums) program
                                               :before)
          (str/starts-with? l "After")  (recur lines (conj samples nums) program
                                               :after)
          :else
          (case last-seen
            (:blank :numbers) (recur lines samples (conj program nums)
                                     :numbers)
            :before           (recur lines (conj samples nums) program
                                     :numbers)))))))

;;; The opcodes for the "machine".

(defn- op-addr [[o A B C] rs] (assoc rs C (+ (rs A) (rs B))))

(defn- op-addi [[o A B C] rs] (assoc rs C (+ (rs A) B)))

(defn- op-mulr [[o A B C] rs] (assoc rs C (* (rs A) (rs B))))

(defn- op-muli [[o A B C] rs] (assoc rs C (* (rs A) B)))

(defn- op-banr [[o A B C] rs] (assoc rs C (bit-and (rs A) (rs B))))

(defn- op-bani [[o A B C] rs] (assoc rs C (bit-and (rs A) B)))

(defn- op-borr [[o A B C] rs] (assoc rs C (bit-or (rs A) (rs B))))

(defn- op-bori [[o A B C] rs] (assoc rs C (bit-or (rs A) B)))

(defn- op-setr [[o A B C] rs] (assoc rs C (rs A)))

(defn- op-seti [[o A B C] rs] (assoc rs C A))

(defn- op-gtir [[o A B C] rs] (assoc rs C (if (> A (rs B)) 1 0)))

(defn- op-gtri [[o A B C] rs] (assoc rs C (if (> (rs A) B) 1 0)))

(defn- op-gtrr [[o A B C] rs] (assoc rs C (if (> (rs A) (rs B)) 1 0)))

(defn- op-eqir [[o A B C] rs] (assoc rs C (if (= A (rs B)) 1 0)))

(defn- op-eqri [[o A B C] rs] (assoc rs C (if (= (rs A) B) 1 0)))

(defn- op-eqrr [[o A B C] rs] (assoc rs C (if (= (rs A) (rs B)) 1 0)))

;;; End of opcodes

;; A list (thus iterable) of all the opcodes.
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

;; Test a sample against the given opcode. Return true/false as to whether the
;; sample could have been this opcode.
(defn- test-sample [sample op]
  (let [[before command after] sample]
    (= (op command before) after)))

;; Problem 1

;; Count how many of the opcodes the sample could be
(defn- count-matches [sample]
  (count (filter #(test-sample sample %) (vals opcodes))))

;; Parse the content of "file" into the set of samples and the sample program.
;; For the first problem, ignore the program and just count how many of the
;; samples could be one of three or more opcodes.
(defn p01 [file]
  (as-> file $
    (read-lines $)
    (parse-input $)
    (map count-matches (:samples $))
    (filter #(> % 2) $)
    (count $)))

;; Problem 2

;; Get the matching opcodes for the sample and return them as a set.
(defn- get-matches [sample]
  (set (filter #(test-sample sample (get opcodes %)) (keys opcodes))))

;; Analyze all the samples and create a vector of sets. For each vector index,
;; the elements of the set are the possible opcodes for that number.
(defn- analyze-samples [samples]
  (let [opmap (vec (repeat 16 #{}))]
    (loop [[sample & samples] samples, opmap opmap]
      (cond
        (nil? sample) opmap
        :else
        (let [[_ [op] _] sample, matches (get-matches sample)]
          (recur samples (assoc opmap op (apply conj (opmap op) matches))))))))

;; Remove all instances of opcode from all of the sets within opvec.
(defn- clear-opcode [opvec opcode]
  (vec (map #(if (set? %) (disj % opcode) %) opvec)))

;; Create the vector mapping opcode numbers to the opcodes themselves. This
;; will yield a vector in which index i holds the pointer to the relevant
;; opcode fn.
(defn- create-opcode-vec [samples]
  (loop [opvec (analyze-samples samples)]
    (let [remaining (count (filter set? opvec))]
      (cond
        (zero? remaining) (vec (map opcodes opvec))
        :else
        (let [candidate (first (filter #(= 1 (count (opvec %)))
                                       (range (count opvec))))
              opcode    (first (opvec candidate))]
          (recur (assoc (clear-opcode opvec opcode) candidate opcode)))))))

;; Run the given program using the map of opcodes given. Assume the four
;; registers all start at 0s.
(defn- run-program [opvec program]
  (loop [[line & lines] program, registers [0 0 0 0]]
    (cond
      (nil? line) registers
      :else       (recur lines ((opvec (first line)) line registers)))))

;; For the second problem, parse both the samples and the sample program. Use
;; the samples to derive the 16 opcodes by their numbers, then use that data to
;; run the given program.
(defn p02 [file]
  (as-> file $
    (read-lines $)
    (parse-input $)
    (run-program (create-opcode-vec (:samples $)) (:program $))))
