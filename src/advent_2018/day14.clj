(ns advent-2018.day14)

;;; https://adventofcode.com/2018/day/14

;; Extend a recipes vector by one or two elements, depending on whether the
;; given number is 10 or greater.
(defn- extend-recipes [r n]
  (if (> n 9)
    (into r [1 (rem n 10)])
    (conj r n)))

;; Problem 1

;; Run the simulation until the recipes vector is at least (elf-target+10)
;; long. Then return the resulting vector.
(defn- simulate-recipes [recipes elf1 elf2 elf-target]
  (let [target (+ 10 elf-target)]
    (loop [r recipes, e1 elf1, e2 elf2]
      (cond
        (<= target (count r)) r
        :else
        (let [new-recipes (extend-recipes r (+ (r e1) (r e2)))
              new-count   (count new-recipes)
              new-elf1    (mod (+ e1 (inc (r e1))) new-count)
              new-elf2    (mod (+ e2 (inc (r e2))) new-count)]
          (recur new-recipes new-elf1 new-elf2))))))

;; Taking the target number, run the recipes simulation until the recipes
;; vector is long-enough to pull out the desired 10 projected recipes.
(defn p01 [target]
  (as-> target $
    (simulate-recipes [3 7] 0 1 $)
    (subvec $ target (+ target 10))
    (apply str $)))

;; Problem 2

;; This is a convenience thing for mapping a digit character to the actual int
;; value.
(def ^:private num-map {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9})

;; Run the simulation as above, but this time we're looking for the target value
;; as a sub-vector of the recipes vector. When it is found, return the number of
;; recipes to the left of it.
(defn- simulate-recipes-2 [recipes elf1 elf2 elf-target]
  (let [target   (vec (map num-map (str elf-target)))
        min-size (count target)]
    (loop [r recipes, e1 elf1, e2 elf2]
      (let [new-sum     (+ (r e1) (r e2))
            new-recipes (extend-recipes r new-sum)
            new-count   (count new-recipes)
            new-elf1    (mod (+ e1 (inc (r e1))) new-count)
            new-elf2    (mod (+ e2 (inc (r e2))) new-count)]
        (cond
          (and (> new-count min-size)
               (> new-sum 9)
               (= target
                  (subvec new-recipes
                          (- new-count min-size 1)
                          (dec new-count))))        (- new-count min-size 1)
          (and (>= new-count min-size)
               (= target
                  (subvec new-recipes
                          (- new-count min-size)))) (- new-count min-size)
          :else
          (recur new-recipes new-elf1 new-elf2))))))

;; Simpler than p01... just run the above simulation. The return value from it
;; is what we want for an answer.
(defn p02 [target]
  (simulate-recipes-2 [3 7] 0 1 target))
