(ns advent-2018.day13
  (:require [clojure.java.io :as io]))

;;; https://adventofcode.com/2018/day/13

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; These maps and sets are used for fast lookup of character types and turn
;; directions.
(def ^:private cart          #{\^ \v \< \>})
(def ^:private corner        #{\/ \\})
(def ^:private cart-to-track {\^ \|, \v \|, \< \-, \> \-})
(def ^:private next-turn     {\r \l, \l, \s, \s \r})
(def ^:private cart-turn-map {[\^ \l] \<, [\^ \s] \^, [\^ \r] \>,
                              [\v \l] \>, [\v \s] \v, [\v \r] \<,
                              [\< \l] \v, [\< \s] \<, [\< \r] \^,
                              [\> \l] \^, [\> \s] \>, [\> \r] \v})
(def ^:private cart-corners  {[\^ \\] \<, [\^ \/] \>,
                              [\v \\] \>, [\v \/] \<,
                              [\< \\] \^, [\< \/] \v,
                              [\> \\] \v, [\> \/] \^})

;; Parse a single line of the input, producing one row for the field and
;; possibly adding to the running vector of carts.
(defn- parse-one-line [line y carts]
  (loop [[c & line] line, row [], x 0, carts carts]
    (cond
      (nil? c) (list row carts)
      (cart c) (recur line (conj row (cart-to-track c)) (inc x)
                      (conj carts {:id (count carts), :dir c,
                                   :x x, :y y, :turn \r}))
      :else    (recur line (conj row c) (inc x) carts))))

;; Parse the full input line-wise. Build up the field a row at a time while
;; recording any carts encountered.
(defn- parse-input [lines]
  (loop [[line & lines] lines, y 0, field [], carts []]
    (cond
      (nil? line) {:field field, :carts carts}
      :else       (let [[new-line new-carts] (parse-one-line line y carts)]
                    (recur lines (inc y) (conj field new-line) new-carts)))))

;; Order the carts for moving.
(defn- order-carts [carts]
  (vec (sort-by :y (sort-by :x carts))))

;; Determine if the given cart (which has just moved) has collided with any of
;; the other carts.
(defn- find-collision [cart carts]
  (loop [[c & cs] carts]
    (cond
      (nil? c)                   nil
      (= (:id cart) (:id c))     (recur cs)
      (= (:dir c) \X)            (recur cs)
      (and (= (:x c) (:x cart))
           (= (:y c) (:y cart))) {:carts [(:id cart) (:id c)]
                                  :x     (:x c)
                                  :y     (:y c)}
      :else                      (recur cs))))

;; Move a single cart one place.
(defn- move-cart [cart field]
  (if (= (:dir cart) \X)
    cart
    (let [{id :id,
           dir :dir,
           x :x,
           y :y,
           turn :turn} cart
          c            (get-in field [y x])
          new-turn     (if (= c \+) (next-turn turn) turn)
          new-dir      (cond
                         (corner c) (cart-corners [dir c])
                         (= c \+)   (cart-turn-map [dir new-turn])
                         :else      dir)
          new-x        (cond
                         (= new-dir \>) (inc x)
                         (= new-dir \<) (dec x)
                         :else          x)
          new-y        (cond
                         (= new-dir \^) (dec y)
                         (= new-dir \v) (inc y)
                         :else          y)]
      {:id id, :dir new-dir, :x new-x, :y new-y, :turn new-turn})))

;; Ensure that all carts that have collided in the current tick are marked as
;; "gone" (the :dir key set to "X").
(defn- clear-carts [carts collisions]
  (let [broken (flatten (map :carts collisions))]
    (vec (reduce (fn [cs c]
                   (assoc-in cs [c :dir] \X))
                 carts broken))))

;; Move all the carts one step on the field. Skips any carts that are "gone"
;; (who's :dir key is "X").
(defn- move-carts [carts field]
  (let [ordered-carts (order-carts carts)]
    (loop [[c & cs]   ordered-carts
           new-carts  carts
           collisions ()]
      (cond
        (nil? c)        (list (clear-carts new-carts collisions)
                              (reverse collisions))
        (= (:dir c) \X) (recur cs new-carts collisions)
        :else
        (let [new-c (move-cart c field)
              crash (find-collision new-c new-carts)]
          (cond
            (nil? crash) (recur cs (assoc new-carts (:id new-c) new-c)
                                collisions)
            :else
            (let []
              (recur cs (assoc new-carts (:id new-c)
                               (assoc new-c :dir \X))
                     (cons crash collisions)))))))))

;; Problem 1

;; Return the first crash that occurs. Moves carts until the crashes list is
;; non-empty, then returns the head of that list.
(defn- find-first-crash [{field :field, carts :carts}]
  (loop [carts carts]
    (let [[new-carts crashes] (move-carts carts field)]
      (cond
        (empty? crashes) (recur new-carts)
        :else            (first crashes)))))

;; The content of "file" is the map of the tracks with the carts on them. For
;; this problem, find the coordinates of the first crash that occurs.
(defn p01 [file]
  (->> file
       (read-lines)
       (parse-input)
       (find-first-crash)))

;; Problem 2

;; Return the cart struct for the last remaining cart after all others have
;; been removed due to crashing.
(defn- last-cart-standing [{field :field, carts :carts}]
  (loop [tick 0, carts carts]
    (let [new-tick    (inc tick)
          [new-carts] (move-carts carts field)
          live-carts  (filter #(not= (:dir %) \X) new-carts)]
      (cond
        (= 1 (count live-carts)) (first live-carts)
        :else                    (recur new-tick new-carts)))))

;; The content of "file" is the map of the tracks with the carts on them. For
;; this problem, find the coordinates of the last cart remaining after all
;; others have crashed and been removed from the field.
(defn p02 [file]
  (->> file
       (read-lines)
       (parse-input)
       (last-cart-standing)))
