(ns advent.day4
  (:require [advent.core :refer [get-input]]
            [clojure.set :as s]
            [clojure.string :as str]))

(def input (get-input 4 #"\n"))

(defn contains-or-contained [ranges]
  (->> ranges
       (map set)
       (apply (juxt s/superset? s/subset?))
       (some true?)))

(defn str-to-ranges [str-input]
  (let [pattern #"(\d*)-(\d*),(\d*)-(\d*)"
        [s1 e1 s2 e2] (map #(Integer. %) (rest (re-matches pattern str-input)))] 
    [(range s1 (inc e1)) (range s2 (inc e2))]))

(defn overlapping? [[r1 r2]]
  (not (empty? (s/intersection (set r1) (set r2)))))

(def day4-pt1 (->> input
                   (map str-to-ranges)
                   (map contains-or-contained)
                   (filter true?)
                   count))

(def day4-pt2 (->> input
                   (map str-to-ranges)
                   (map overlapping?)
                   (filter true?)
                   count))
