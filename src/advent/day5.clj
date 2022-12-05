(ns advent.day5
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]))

(defn parse-input [input]
  (let [[pt1 _ moves] (partition-by empty? (str/split input #"\n"))
        stacks (butlast pt1)]
    [stacks moves]))

(defn parse-move [input]
  (let [matches (re-matches #"\D*(\d+)\D*(\d+)\D*(\d+)" input)]
    (map #(Integer. %) (rest matches))))

(defn parse-stacks-row [row]
  (->> row
       (partition 3 4)
       (map second)))

(defn parse-stacks [rows]
  (let [parsed (->> rows
                    (map parse-stacks-row)
                    (apply map vector)
                    (map reverse))]
    (for [stack parsed] (filterv (partial not= \space) stack))))

(defn move [stacks [move-count from to]]
  (let [from-stack (nth stacks (dec from))
        number-to-leave (- (count from-stack) move-count)
        in-crane (drop number-to-leave from-stack)
        pickedup (update-in (vec stacks) [(dec from)] #(vec (take number-to-leave %)))]
    (update-in pickedup [(dec to)] into in-crane)))

(defn unfold-moves [[move-count from to]]
  (let [base-move [1 from to]]
    (map (constantly [1 from to]) (range move-count))))

(defn day5 [stacks moves]
  (->> moves
       (reduce move stacks)
       (map last)
       (str/join)))
(def input (parse-input (get-input 5)))
(def stacks (parse-stacks (get input 0)))
(def moves (map parse-move (get input 1)))

(def day5-pt1 (day5 stacks (mapcat unfold-moves moves)))
(def day5-pt2 (day5 stacks moves))
