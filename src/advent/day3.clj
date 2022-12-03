(ns advent.day3
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.set :as s]))

(def rucksacks (get-input 3 #"\n"))

(defn compartmentalize [items]
  (map #(apply str %) (partition-all (/ (count items) 2) items)))

(defn priority-of-item [item]
  (let [to-int (int item)
        a (int \a)
        A (int \A)]
    (if (>= to-int a)
      (+ 1 (- to-int a))
      (+ 27 (- to-int A)))))

(def sum (partial reduce +))

(defn find-common [[compartment-1 compartment-2]]
  (let [intersection (s/intersection (set compartment-1) (set compartment-2))]
    intersection))

(defn day3-pt1 [input]
  (let [compartments (map compartmentalize input)
        common (map find-common compartments)
        prioritized (for [items common] (map priority-of-item items))
        summed-rucksacks (map sum prioritized)]
    (sum summed-rucksacks)))

(def group-elves (partial partition 3))
(defn group-badge [[& rucksacks]]
  (apply s/intersection (map set rucksacks)))

(defn day3-pt2 [input]
  (let [groups (group-elves input)
        with-badges (map group-badge groups)
        flattened (flatten (map vec with-badges))
        prioritized (map priority-of-item flattened)]
    (sum prioritized)))

