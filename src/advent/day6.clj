(ns advent.day6
  (:require [advent.core :refer [get-input]]))

(defn start-marker? [marker-length]
  (fn [chars]
    (= marker-length (count (distinct chars)))))

(defn find-start [marker-length input]
  (+ marker-length (count (->> input
                               (partition marker-length 1)
                               (take-while (complement (start-marker? marker-length)))))))

(def day6-pt1 (find-start 4 (get-input 6)))
(def day6-pt2 (find-start 14 (get-input 6)))
