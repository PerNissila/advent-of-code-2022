(ns advent.day10
  (:require [clojure.string :as st]
            [advent.core :refer [get-input]]))

(defn parse-op [op]
  (st/split op #" "))

(defn cycle
  ([[op & ops] X]
   (if (not op)
     X
     (lazy-seq
      (let [[op args] (parse-op op)
            last-x (or (peek X) 1)]
        (cond
          (= op "noop") (cycle ops (conj X last-x))
          (= op "addx") (cycle (cons (str "setx " args) ops) (conj X last-x))
          (= op "setx") (cycle ops (conj X (+ last-x (parse-long args))))
          )))))
  ([ops]
   (cycle ops [1])))

(def partitions [20 60 100 140 180 220])

(def input (get-input 10 #"\n"))
(->> partitions
     (map #(take % (cycle input)))
     (map (juxt count last))
     (map #(apply * %))
     (apply +))

(defn draw [line]
  (prn)
  (loop [line line]
    (when (seq line)
      (let [[position X] (first line)
            on-sprite? (and (<= (dec X) position (inc X)))]
        (if on-sprite?
          (print "#")
          (print " "))
        (recur (rest line))))))

(defn get-line [register]
  (map vector (range) register))

;; Draw part 2 
(->> (partition 40 (cycle input))
     (map get-line)
     (map draw)
     dorun)

