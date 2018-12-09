(ns advent-2018.day08)

;;; https://adventofcode.com/2018/day/8

;; Build a node and return it, along with the updated index into data. Use
;; direct recursion (not recur) to handle children.
(defn- build-node [idx data]
  (let [cntc (data idx)
        cntm (data (inc idx))]
    (loop [child 0, index (+ 2 idx), children ()]
      (cond
        (= child cntc) (list {:children (reverse children)
                              :metadata (map data
                                             (range index
                                                    (+ index cntm)))}
                             (+ index cntm))
        :else
        (let [[newchild newindex] (build-node index data)]
          (recur (inc child) newindex (cons newchild children)))))))

;; Create the tree by kicking off build-node on index 0 of data.
(defn- create-tree [data]
  (let [data (vec (map #(Integer/parseInt %) (re-seq #"\d+" data)))]
    (first (build-node 0 data))))

;; Problem 1

;; Count all the node metadata recursively.
(defn- count-metadata [node]
  (let [{children :children
         metadata :metadata} node]
    (+ (apply + metadata)
       (apply + (map count-metadata children)))))

;; The data in "file" is the sequence of numbers that describe the tree
;; structure in a recursive manner. Build the tree and sum up all the
;; metadata values.
(defn p01 [file]
  (->> file
       (slurp)
       (create-tree)
       (count-metadata)))

;; Problem 2

;; Checksum works like this:
;;
;; * If the node has no children, it's value is the sum of metadata values
;; * If the node has children, each metadata value is an index into the
;;   list of children. The value of the node is the sum of the values of
;;   the child nodes.
;; * A metadata value of 0 or in excess of the number of children is
;;   ignored.
;; * If the metadata is a duplicate value, it counts that many times.
(defn- checksum-metadata [node]
  (let [{children :children
         metadata :metadata} node
        children (vec children)
        num      (count children)]
    (cond
      (zero? (count children)) (apply + metadata)
      :else
      (apply + (map #(cond
                       (zero? %) 0
                       (> % num) 0
                       :else     (checksum-metadata (children (dec %))))
                    metadata)))))

;; The data in "file" is the sequence of numbers that describe the tree
;; structure in a recursive manner. Build the tree and calculate the
;; "checksum" according to the instructions on the problem page.
(defn p02 [file]
  (->> file
       (slurp)
       (create-tree)
       (checksum-metadata)))
