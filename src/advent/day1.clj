(ns advent.day1
  (:require [advent.core :as advent]
            [clojure.string :as str]))

(defn day1 []
  (let [input (advent/get-input 1)
        numbers (str/split input #"\n")
        parsed (map #(Integer/parseInt (case % "" "0" %)) numbers)
        grouped (map #(reduce + %) (partition-by zero? parsed))
        filtered (filter (complement zero?) grouped)]
    filtered))

(defn day1-pt1 []
  (apply max (day1)))

(defn day1-pt2 []
  (reduce + (take 3 (sort > (day1)))))
