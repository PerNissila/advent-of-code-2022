(ns advent.day4
  (:require [advent.core :refer [get-input]]
            [clojure.set :as s]
            [clojure.string :as str]))

(defn contains [r1 r2]
  (s/superset? (set r1) (set r2)))
(defn contained [r1 r2]
  (contains r2 r1))
(def contains-or-contained
  (comp boolean
        (partial some boolean)
        (juxt contains contained)))

(defn str-to-ranges [str-input]
  (let [pattern #"(\d*)-(\d*),(\d*)-(\d*)"
        [s1 e1 s2 e2] (map #(Integer. %) (rest (re-matches pattern str-input)))] 
    [(range s1 (inc e1)) (range s2 (inc e2))]))


(def input (get-input 4 #"\n"))
(def day4-pt1 (->> input
                   (map str-to-ranges)
                   (map #(apply contains-or-contained %))
                   (filter boolean)
                   count))

(defn overlapping? [[r1 r2]]
  (not (empty? (s/intersection (set r1) (set r2)))))

(def day4-pt2 (->> input
                   (map str-to-ranges)
                   (map overlapping?)
                   (filter boolean)
                   count))
