(ns advent.day9
  (:require [advent.core :refer [get-input]]
            [clojure.string :as st]))

(defn unfold-move [[dir _ & steps]]
  (repeat (parse-long (st/join steps)) [dir 1]))


(defn move [[x y] dir]
  (case dir
    \U [x (inc y)]
    \D [x (dec y)]
    \R [(inc x) y]
    \L [(dec x) y]))

(defn adjacent? [c1 c2]
  (not (some #(> (abs %) 1) (map - c1 c2))))

(defn next-tail-pos [[head-x head-y] [tail-x tail-y]]
  (let [diff-x (- head-x tail-x)
        diff-y (- head-y tail-y)]
    (if (< (max (abs diff-x) (abs diff-y)) 2)
      [tail-x tail-y]
      [(+ tail-x (cond
                   (pos? diff-x) 1
                   (neg? diff-x) -1
                   :else 0))
       (+ tail-y (cond
                   (pos? diff-y) 1
                   (neg? diff-y) -1
                   :else 0))])))

(defn update-tails [rope tail]
  (let [to-follow (peek rope)]
    (if (adjacent? to-follow tail)
      (conj rope tail)
      (conj rope (next-tail-pos to-follow tail))))
  )

(defn next-state [{:keys [rope visited start]} dir]
  (let [[head & tails] rope
        next-head (move head dir)
        updated (reduce update-tails [next-head] tails)]
    {:rope updated
     :visited (conj visited (peek updated))
     :start start}))

(defn create-rope [length]
  {:rope (repeat length [0 0])
   :visited #{[0 0]}
   :start [0 0]})

(def pt1
  (->> (get-input 9 #"\n")
       (mapcat unfold-move)
       (map first)
       (reduce next-state (create-rope 2))
       :visited
       count))

(def pt2
  (->> (get-input 9 #"\n")
       (mapcat unfold-move)
       (map first)
       (reduce next-state (create-rope 10))
       :visited
       count))
