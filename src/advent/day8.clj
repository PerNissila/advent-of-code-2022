(ns advent.day8
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]))


(def input (get-input 8 #"\n"))

(defn string-to-longs [row]
  (mapv (comp parse-long str) row))

(defn trees< [[& trees] tree]
  (every? (partial > tree) trees))

(defn get-column [trees x]
  (mapv #(get % x) trees))

(defn get-trees-around [trees index]
  [(take index trees) (drop (inc index) trees)])

(defn visible-from-outside? [trees index]
  (let [tree (get trees index)
        [side1 side2] (get-trees-around trees index)]
    (or (trees< side1 tree) (trees< side2 tree))))

(defn scan-grid [cell-fn grid]
  (for [y (range (count grid))
        x (range (count (get grid 0)))]
    (let [row (get grid y)
          col (get-column grid x)]
      [(cell-fn row x) (cell-fn col y)])))

(defn take-while-including [pred coll]
  (lazy-seq
   (let [[next] coll]
     (when next
       (if (pred next)
         (cons next (take-while-including pred (rest coll)))
         [next])))))

(defn visible-trees-from-inside? [trees index]
  (let [tree (get trees index)
        [side1 side2] (get-trees-around trees index)]
    [(* (count (take-while-including (partial > tree) (reverse side1)))
        (count (take-while-including (partial > tree) side2)))]))

(def pt1
  (->> input
       (mapv string-to-longs)
       (scan-grid visible-from-outside?)
       (map (partial some true?))
       (filter true?)
       count))

(def pt2
  (->> input
       (mapv string-to-longs)
       (scan-grid visible-trees-from-inside?)
       (map flatten)
       (map (partial reduce *))
       (apply max)))
