(ns advent-2018.day24
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;;; https://adventofcode.com/2018/day/24

;; Read all the lines in the given file. Return a list of lines. "doall" is
;; needed to fully-realize the lazyseq before the filehandle is closed.
(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

;; Parse the damage-modifiers part of the line.
(defn- parse-mods [modstr]
  (let [immunestr (last (re-find #"immune to ([\w, ]+)[;)]" modstr))
        weakstr   (last (re-find #"weak to ([\w, ]+)[;)]" modstr))]
    {:immune-to (when immunestr (reduce (fn [m k]
                                          (assoc m (keyword k) true))
                                        {} (str/split immunestr #", ")))
     :weak-to   (when weakstr (reduce (fn [m k]
                                        (assoc m (keyword k) true))
                                      {} (str/split weakstr #", ")))}))

;; Parse a single line into a struct that represents a group for one side or
;; the other.
(def ^:private line-re
  (re-pattern (str "(\\d+) units each with (\\d+) hit points (\\([^)]*\\) )?"
                   "with an attack that does (\\d+) (\\w+) damage at "
                   "initiative (\\d+)")))
(defn- parse-line [side id gid line]
  (let [parts (vec (rest (re-find line-re line)))
        mods  (if (parts 2) (parse-mods (parts 2)) {})]
    {:id    id
     :gid   gid
     :side  side
     :units (Integer/parseInt (parts 0))
     :hp    (Integer/parseInt (parts 1))
     :dmg   (Integer/parseInt (parts 3))
     :type  (keyword (parts 4))
     :mods  mods
     :init  (Integer/parseInt (parts 5))}))

;; Parse the input into a vector of all combat groups. Later, they'll be
;; sorted into sides.
(defn- parse-input [lines]
  (loop [[l & lines] lines, groups [], current nil, id 0, gid 0]
    (cond
      (nil? l)                      groups
      (empty? (str/trim-newline l)) (recur lines groups nil id 0)
      (str/starts-with? l "Immune") (recur lines groups :immune id 1)
      (str/starts-with? l "Infect") (recur lines groups :infect id 1)
      :else
      (recur lines (conj groups (parse-line current id gid l))
             current (inc id) (inc gid)))))

;; Compute the effective power of the given group g.
(defn- effective-power [g] (* (:units g) (:dmg g)))

;; Compute the effective damage one group (g1) would inflict on the other (g2).
(defn- effective-damage [g1 g2]
  (let [base-dmg  (effective-power g1)
        base-type (:type g1)
        mult      (if (get-in g2 [:mods :weak-to base-type] nil) 2 1)
        mult      (if (get-in g2 [:mods :immune-to base-type] nil) 0 mult)]
    (* base-dmg mult)))

;; Determine if combat is finished by checking to see if there are still two
;; sides with active groups.
(defn- finished? [groups]
  (= 1 (count (distinct (map :side (filter #(pos? (:units %)) groups))))))

;; Sort routine for targeting order. Order is by effective power first, then
;; ties broken by initiative. Both descending.
(defn- targeting-order [a b]
  (if (= (:ep a) (:ep b))
    (compare (:init b) (:init a))
    (compare (:ep b) (:ep a))))

;; Choose a target for the given attacker from the vector of targets. Make
;; sure the target has not already been targeted by checking the targeted
;; set.
(defn- choose-target [attacker targets targeted]
  (let [valid (filter #(not (targeted (:id %))) targets)
        valid (map #(hash-map :id   (:id %)
                              :gid  (:gid %)
                              :aid  (:id attacker)
                              :ep   (effective-power %)
                              :ed   (effective-damage attacker %)
                              :init (:init %)) valid)]
    (last (->> valid
               (filter #(pos? (:ed %)))
               (sort-by :init)
               (sort-by :ep)
               (sort-by :ed)))))

;; Get the targeting for the two groups. In their order of effective power
;; (decreasing), each group will choose an opposing group. For each of these,
;; insert a map with (:attacker, :target, :init) into the vector that gets
;; returned.
(defn- get-targeting [immune infect]
  (let [all (map #(assoc % :ep (effective-power %)) (concat immune infect))
        all (sort targeting-order all)]
    (loop [[attacker & all] all, targeting [], targeted #{}]
      (cond
        (nil? attacker) (group-by :aid targeting)
        :else
        (let [targets (if (= (:side attacker) :immune) infect immune)
              target  (choose-target attacker targets targeted)]
          (recur all (conj targeting target) (conj targeted (:id target))))))))

;; Determine the order in which all the units will attack. The order of attack
;; is determined by initiative, decreasing.
(defn- get-attack-order [immune infect]
  (map :id (sort #(compare (:init %2) (:init %1)) (concat immune infect))))

;; Apply damage to a given unit-group. A group only loses whole units, so any
;; leftover damage is ignored. Return the new struct the :units key updated.
(defn- apply-damage [attack target]
  (let [{units :units
         hp    :hp}   target
        damage        (effective-damage attack target)
        killed        (quot damage hp)]
    (assoc target :units (max 0 (- units killed)))))

;; Apply one round of attack, returning a modified vector of the groups. The
;; order and such are the same, but the individual entries reflect the
;; results of the combat.
(defn- apply-attack [groups attack targets]
  (loop [[a-id & as] attack, groups groups]
    (cond
      (nil? a-id) groups
      :else
      (let [atk    (groups a-id)
            trg    (first (targets a-id))
            t-id   (:id trg)
            target (if-not (nil? t-id) (groups t-id))]
        (cond
          (zero? (:units atk))    (recur as groups)
          (nil? target)           (recur as groups)
          (zero? (:units target)) (recur as groups)
          :else
          (recur as (assoc groups t-id (apply-damage atk target))))))))

;; Apply the given boost to the damage rating of all "immune system" groups.
(defn- boost-immune [groups boost]
  (mapv #(if (= (:side %) :immune) (update % :dmg + boost) %) groups))

;; Run the immune system combat.
(defn- immune-combat [boost groups]
  (let [groups (boost-immune groups boost)]
    (loop [groups groups, last-groups nil]
      (cond
        (= groups last-groups) nil
        (finished? groups)     groups
        :else
        (let [{immune :immune
               infect :infect} (group-by :side groups)
              immune           (filter #(pos? (:units %)) immune)
              infect           (filter #(pos? (:units %)) infect)
              targeting        (get-targeting immune infect)
              attacking        (get-attack-order immune infect)]
          (recur (apply-attack groups attacking targeting) groups))))))

;; Problem 1

;; Tally the results, the number of units left on the winning team. Which, since
;; the other team has been completely eliminated, means just adding up the
;; :units values of all of them.
(defn- results [groups] (apply + (map :units groups)))

;; Using the setup given in "file", run the immune system combat simulation
;; with a "boost" of zero. The return value should be the count of remaining
;; units on the winning side.
(defn p01 [file]
  (->> file
       (read-lines)
       (parse-input)
       (immune-combat 0)
       (results)))

;; Problem 2

;; Determine if the immune system has won.
(defn- immune-win? [groups]
  (= (list :immune) (distinct (map :side (filter #(pos? (:units %)) groups)))))

;; Find the starting high-value for the binary search in find-boost. Starts
;; with an offset of 1 from the given low value and doubles this each iteration
;; until the combat result is a different winner than the low value produced.
(defn- find-hi [lo-bool lo groups]
  (loop [offset 1]
    (cond
      (= lo-bool
         (immune-win? (immune-combat (+ lo offset)
                                     groups)))     (recur (* 2 offset))
      :else                                        (+ lo offset))))

;; Do a binary search over a range of potential boost values. Sets the high
;; end of the range by probing from the low value in offsets that are powers
;; of 2. Returns the boost value that should be the minimum needed to get
;; a win for the immune system.
(defn- find-boost [groups]
  (loop [lo-bool nil, lo 0, hi nil, best nil]
    (cond
      (nil? lo-bool) (recur (immune-win? (immune-combat lo groups))
                            lo hi best)
      (nil? hi)      (let [hi (find-hi lo-bool lo groups)]
                       (recur lo-bool lo hi (if lo-bool lo hi)))
      (<= lo hi)     (let [mid    (quot (+ lo hi) 2)
                           result (immune-win? (immune-combat mid groups))
                           best   (if result mid best)]
                       (cond
                         (= result lo-bool) (recur lo-bool (inc mid) hi best)
                         :else              (recur lo-bool lo (dec mid) best)))
      :else          best)))

;; Find the minimum boost value that ensures a win for the immune system, then
;; run the combat with that value to get the proper results value.
(defn- find-min-win [groups]
  (let [boost (find-boost groups)]
    (->> groups
         (immune-combat boost)
         (results))))

;; Using the setup given in "file", run the immune system combat simulation
;; after finding the minimal boost value that will guarantee a win for the
;; immune system side. The return value should be the count of remaining units
;; on the winning side.
(defn p02 [file]
  (->> file
       (read-lines)
       (parse-input)
       (find-min-win)))
