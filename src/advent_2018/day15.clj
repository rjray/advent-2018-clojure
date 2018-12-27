(ns advent-2018.day15
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/15

;;; NOTE: This one has a lot of commented-out console display code. These
;;; were vital in debugging this, and I didn't want to delete them entirely.

;; Simple map to ease the determination of who the enemy of the person under
;; consideration is.
(def ^:private enemy {\G \E, \E \G})
;; The order in which squares should be considered when moving/targeting.
(def ^:private square-order (list [0 -1] [-1 0] [1 0] [0 1]))

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Sort the given structs (points, people, etc.) in "reading" order by the :x
;; and :y attributes on each.
(defn- reading-order [structs] (sort-by :y (sort-by :x structs)))

;;; Implementation of the A* path-finding alhorithm. Most of this taken/adapted
;;; from: https://nakkaya.com/2010/06/01/path-finding-using-astar-in-clojure/

(defn a*-manhattan-distance [[x1 y1] [x2 y2]]
  (+ (Math/abs ^Integer (- x2 x1)) (Math/abs ^Integer (- y2 y1))))

(defn a*-cost [curr end pg]
  (let [g (inc pg)
        h (a*-manhattan-distance curr end)
        f (+ g h)]
    [f g h]))

(defn a*-offsets [map width height closed x y]
  (for [rx (range -1 2)
        ry (range -1 2)
        :when
        (let [tx (+ x rx), ty (+ y ry)]
          (and (not= (Math/abs ^Integer rx) (Math/abs ^Integer ry))
               (>= tx 0)
               (>= ty 0)
               (< tx width)
               (< ty height)
               (not (contains? closed [tx ty]))
               (= (get-in map [ty tx]) \.)))]
    [rx ry]))

(defn a*-edges [field width height closed coord]
  (let [[x y]   coord
        offsets (a*-offsets field width height closed x y)]
    (map #(vec (map + coord %)) offsets)))

(defn- a*-path [end parent closed]
  (if (nil? parent)
    (list end)
    (reverse
     (loop [path [end parent]
            node (closed parent)]
       (if (nil? node)
         path
         (recur (conj path node) (closed node)))))))

(defn- a*-pri-sort-fn [[_ [x _]] [_ [y _]]]
  (if (= x y)
    0
    (let [[f1 _ h1] x
          [f2 _ h2] y]
      (if (= f1 f2)
        (if (< h1 h2) -1 1)
        (if (< f1 f2) -1 1)))))

(defn- a*-priority-sort [coll]
  (sort a*-pri-sort-fn coll))

(defn- a*-search
  ([map start end]
   (let [[sx sy] start
         [ex ey] end
         open    {start [(a*-cost start end -1) nil]}
         closed  {}
         width   (count (first map))
         height  (count map)]
     (a*-search map width height open closed start end)))

  ([map width height open closed start end]
   (if-let [[coord [[_ prevg] parent]] (first (a*-priority-sort open))]
     (if-not (= coord end)
       (let [closed (assoc closed coord parent)
             edges  (a*-edges map width height closed coord)
             open   (reduce
                     (fn [open edge]
                       (if-not (contains? open edge)
                         (assoc open edge [(a*-cost edge end prevg) coord])
                         (let [[[_ pg] _] (open edge)
                               [nf ng nh] (a*-cost edge end prevg)]
                           (if (< ng pg)
                             (assoc open edge [[nf ng nh] coord])
                             open))))
                     (dissoc open coord) edges)]
         (recur map width height open closed start end))
       (a*-path end parent closed)))))

;;; End of A* block

;; If the looked-at square had a value of "E" or "G", it's a combatant. Create
;; a new structure to be added to the current row-vector.
(defn- create-person [ch x y epwr]
  (if (= ch \G)
    {:type ch, :x x, :y y, :hp 200, :atk 3, :alive true}
    {:type ch, :x x, :y y, :hp 200, :atk epwr, :alive true}))

;; Create a single row in the field from the string of characters.
(defn- create-row [s y epwr]
  (loop [[c & cs] s, x 0, row []]
    (cond
      (nil? c)  row
      (enemy c) (recur cs (inc x) (conj row (create-person c x y epwr)))
      :else     (recur cs (inc x) (conj row c)))))

;; Build the internal representation of the field of combat.
(defn- create-field [elfpower lines]
  (loop [[l & ls] lines, y 0, field []]
    (cond
      (nil? l) field
      :else    (recur ls (inc y)
                      (conj field
                            (create-row (str/trim-newline l) y elfpower))))))

;; Scan the field and create a vector of goblins and elves with their starting
;; coordinates.
(defn- locate-people [field]
  (let [x-max (count (first field))
        y-max (count field)]
    (loop [x 0, y 0, field field, people []]
      (cond
        (= y y-max) (list field people)
        (= x x-max) (recur 0 (inc y) field people)
        :else
        (let [val (get-in field [y x])]
          (cond
            (map? val) (recur (inc x) y
                              (update-in field [y x] assoc :id (count people))
                              (conj people (assoc val :id (count people))))
            :else      (recur (inc x) y field people)))))))

;; Create the map that will be used to manage the scenario.
(defn- create-scenario [field]
  (let [[field people] (locate-people field)]
    {:field  field, :people people, :winner nil}))

;; Execute a single attack from p on t (target). If this kills t, update the
;; field and people values.
(defn- do-attack [p t field people]
  (let [t-id (:id t)
        t-hp (get-in people [t-id :hp])
        t-hp (- t-hp (:atk p))]
    ;;(printf "%c(%2d, %3dhp) at (%d,%d) hits %c(%2d, %3dhp) at (%d,%d)" (:type p) (:id p) (:hp p) (:x p) (:y p) (:type t) (:id t) (:hp t) (:x t) (:y t))
    ;;(if (pos? t-hp) (printf ", reducing it to %dhp\n" t-hp) (print ", killing it\n"))
    ;;(flush)
    (cond
      (pos? t-hp) (list field
                        (assoc-in people [t-id :hp] t-hp))
      :else       (list (assoc-in field [(:y t) (:x t)] \.)
                        (update-in people [t-id] assoc :hp 0 :alive false)))))

;; Get the viable square coordinates around pos. These are those whose value
;; causes f to return true. If there are none, return an empty list.
(defn- viable-squares [pos f field]
  (let [center  (list (:x pos) (:y pos))
        choices (map #(map + center %) square-order)]
    (filter #(f (get-in field (reverse %))) choices)))

;; Return a struct for the data on distance to the given target. The struct
;; will have the actual distance, the (x,y) of the target (for sorting ties),
;; and the list that is the path to the target (so that the head element can be
;; used to move the person).
(defn- dist-to-target [p t field]
  (let [pcoord     [(:x p) (:y p)]
        candidates (viable-squares t (fn [x] (= x \.)) field)
        structs    (map #(hash-map :path (a*-search field pcoord (vec %))
                                   :x    (first %)
                                   :y    (last %))
                        candidates)
        structs    (filter #(not (nil? (:path %))) structs)
        structs    (map #(assoc % :dist (count (:path %))) structs)]
    structs))

;; Find the "preferred" path from person p to the target square t. That is, if
;; there are two ways to go that are the same distance to t, pick the one that
;; is first in "reading order".
(defn- preferred-path [p t field]
  (when-not (nil? t)
    (let [tc     [(:x t) (:y t)]
          points (viable-squares p (fn [x] (= x \.)) field)
          paths  (map #(hash-map :path (a*-search field (vec %) tc)
                                 :x    (first %)
                                 :y    (last %))
                      points)
          paths  (filter #(not (nil? (:path %))) paths)
          paths  (map #(assoc % :dist (count (:path %))) paths)]
      (first (sort-by :dist (reading-order paths))))))

;; Update the given person-struct with the given move.
(defn- move [p [x y]]
  ;; (printf "%c(%2d, %3dhp) at (%d,%d) moves to (%d,%d)\n" (:type p) (:id p) (:hp p) (:x p) (:y p) x y)
  ;;(flush)
  (assoc p :x x :y y))

;; Move the given person one square towards their nearest opponent. Update both
;; the field and full people vector to reflect the movement, as well as the
;; player struct itself.
(defn- do-move [p field people]
  (let [enemy-type (enemy (:type p))
        targets    (filter :alive (filter #(= enemy-type (:type %)) people))]
    (cond
      ;; If there are no targets, no one needs to move
      (empty? targets) (list p field people)
      :else
      (let [structs  (mapcat #(dist-to-target p % field) targets)
            structs  (sort-by :dist (reading-order structs))
            target   (first structs)
            bestpath (preferred-path p target field)
            step     (first (:path bestpath))
            p'       (when-not (nil? step) (move p step))]
        (cond
          (nil? p') (list p field people)
          :else     (list p'
                          (assoc-in (assoc-in field [(:y p) (:x p)] \.)
                                    [(:y p') (:x p')] p')
                          (assoc people (:id p) p')))))))

;; Find any adjacent enemies to the given person.
(defn- find-target [p field people]
  (let [target-type (enemy (:type p))
        targets     (viable-squares p
                                    (fn [x]
                                      (= (:type x) target-type))
                                    field)
        targets     (map #(get-in field (reverse %)) targets)
        targets     (map #(people (:id %)) targets)
        target      (first (sort-by :hp targets))]
    target))

;; Create the string for displaying a combatant in the board-dump.
(defn- dump-person [p]
  (format "%c(%d)" (:type p) (:hp p)))

;; Dump the board/field in its current state.
(defn- dump-board [field people]
  (loop [[l & ls] field]
    (cond
      (nil? l) (flush)
      :else
      (let [newl (map #(if (map? %) (:type %) %) l)
            ppl  (filter map? l)]
        (printf "%s" (apply str newl))
        (if (zero? (count ppl))
          (println)
          (printf "   %s\n"
                  (str/join ", " (map #(dump-person (people (:id %))) ppl))))
        (recur ls)))))

;; Check for the end-of-combat condidtion.
(defn- check-for-end [field people turn]
  (let [elves           (filter #(and (= (:type %) \E) (:alive %)) people)
        goblins         (filter #(and (= (:type %) \G) (:alive %)) people)]
    ;;(dump-board field people)
    (cond
      (zero? (count goblins)) {:winner    "Elves"
                               :turn      turn
                               :remaining (count elves)
                               :hptotal   (apply + (map :hp elves))}
      (zero? (count elves))   {:winner    "Goblins"
                               :turn      turn
                               :remaining 0
                               :hptotal   (apply + (map :hp goblins))}
      :else                   nil)))

;; Run a single round of the combat simulation.
(defn- run-one-round [scen turn]
  (let [{field  :field
         people :people} scen
        players          (map :id (reading-order (filter :alive people)))]
    ;;(printf "Turn %d:\n" turn)
    (loop [[p & ps] players, field field, people people]
      (let [p    (get people p nil)
            fini (check-for-end field people (dec turn))]
        (cond
          (and fini p)     fini
          (nil? p)         (merge scen
                                  {:field field, :people people}
                                  (check-for-end field people turn))
          (not (:alive p)) (recur ps field people)
          :else
          (let [target (find-target p field people)]
            (cond
              target (let [[field' people'] (do-attack p target field people)]
                       (recur ps field' people'))
              :else
              (let [[p' field' people'] (do-move p field people)
                    target              (find-target p' field' people')]
                (cond
                  target (let [[field'' people''] (do-attack p' target
                                                             field' people')]
                           (recur ps field'' people''))
                  :else  (recur ps field' people'))))))))))

;; Run the combat scenario until all units of one class or other are dead.
(defn- run-scenario [scen]
  ;;(print "Initial:\n")
  ;;(dump-board (:field scen) (:people scen))
  (loop [turn 1, scen scen]
    (let [newscen (run-one-round scen turn)]
      (cond
        (:winner newscen) {:winner    (:winner newscen)
                           :hptotal   (:hptotal newscen)
                           :turn      (:turn newscen)
                           :score     (* (:turn newscen) (:hptotal newscen))
                           :remaining (:remaining newscen)}
        :else             (recur (inc turn) newscen)))))

;; Problem 1

;; Run the combat scenario in "file". For this part, assume that all elves
;; have the default attack power of 3.
(defn p01 [file]
  (->> file
       (read-lines)
       (create-field 3)
       (create-scenario)
       (run-scenario)))

;; Problem 2

;; Run the combat scenario with the given attack power value for the elves. If
;; it results in a no-loss victory for the elves, return the victory struct.
(defn- run-with-power [power lines]
  (let [field    (create-field power lines)
        scenario (create-scenario field)
        elves    (filter #(= \E (:type %)) (:people scenario))
        elfcount (count elves)
        result   (run-scenario scenario)]
    (if (= elfcount (:remaining result)) result)))

;; Run the combat scenario in "file". For this part, gradually ramp up the
;; attack power of the elves until they get a no-loss victory.
(defn p02 [file]
  (let [lines (read-lines file)]
    (loop [power 4]
      (let [result (run-with-power power lines)]
        (cond
          (nil? result) (recur (inc power))
          :else         (assoc result :elfpower power))))))
