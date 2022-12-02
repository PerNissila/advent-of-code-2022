(ns advent.day2
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]
            [clojure.set :refer [map-invert]]))


;;;;;;;  pt1
(defonce move-map-pt1 {\A "rock"
               \X "rock"
               \B "paper"
               \Y "paper"
               \C "scissor"
                       \Z "scissor"})

(def winning-moves {"paper" "rock"
                     "scissor" "paper"
                     "rock" "scissor"})

(defn normalize [[elf _ player]]
  [(get move-map-pt1 elf) (get move-map-pt1 player)])

(def input (map normalize (str/split-lines (get-input 2))))

(defn winner? [p1 p2]
  (= p2 (get winning-moves p1)))

(defn get-score [p1 p2]
  (let [item-score {"paper" 2
                    "rock" 1
                    "scissor" 3}
        winner-score (cond (winner? p1 p2) 6
                           (= p1 p2) 3
                           :else 0)]
    (+ (get item-score p1) winner-score)))

(defn get-result [input]
  (reduce + (map (fn [[elf player]] (get-score player elf)) input)))
(def pt1-result
  (get-result input))

;;;;;; pt2
(def losing-moves (map-invert winning-moves))
(def pt2-input (str/split-lines (get-input 2)))

(defn get-move [[elf _ player]]
  (let [elf-move (get move-map-pt1 elf)]
    [elf-move (case player
                              \X (get winning-moves elf-move)
                              \Y elf-move
                              \Z (get losing-moves elf-move))]))

(def pt2-result
  (get-result (map get-move pt2-input)))
