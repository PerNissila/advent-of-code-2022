(ns advent.day7
  (:require [advent.core :refer [get-input]]
            [clojure.string :as str]))

(def input (filter (partial re-matches #"(\$ cd.*|\d+).*") (get-input 7 #"\n")))

(defn get-full-path [[_ & parts]]
  (str "/" (apply str (interpose "/" parts))))

(defn get-sizes [parts size]
  (loop [sizes {}
         parts parts]
    (if (not (seq parts))
      sizes
      (let [full-path (get-full-path parts)]
        (recur (assoc sizes full-path size) (pop parts))))))

(defn parse [input]
  (loop [input input
         sizes {}
         path []]
    (if (not (seq input))
      sizes
      (let [[command & rest-commands] input
            [_ param1 param2] (re-matches #"\$? ?(.*) (.*)" command)]
        (cond
          (= param1 "cd") (case param2
                           ".." (recur rest-commands sizes (pop path))
                           (recur rest-commands sizes (conj path param2)))
          :else (recur rest-commands (merge-with + sizes (get-sizes path (parse-long param1))) path))
        ))))

(def disk-space 70000000)
(def update-size 30000000)

(def filesystem (parse input))

(def max-size 100000)

(def pt1
  (->> filesystem
       (map second)
       (filter (partial >= max-size))
       (reduce +)))

(def pt2 (second (let [available (- disk-space (filesystem "/"))
                       missing (- update-size available)
                       candidates (filter (fn [[_ dir-size]] (>= dir-size missing)) filesystem)]
                   (first (sort-by second < candidates)))))
