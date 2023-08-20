(ns advent.day11
  (:require [advent.core :refer [get-input]]
            [clojure.string :as st]))

(def functions {"*" *
                "+" +
                "-" -})

(defn parse-operand [operand]
  (if (= operand "old")
    :old
    (parse-long operand)))

(defn parse-monkey [string]
  (let [lines (rest (st/split string #"\n"))
        [start operation & test] lines
        [_ items] (re-matches #".*Starting items: (.*)" start)
        [_ operator operand] (re-matches #".*= old (.*) (.*)" operation)
        [_ divisible-by] (re-matches #"\W+Test: divisible by (\d+)" (first test))
        [_ alt-1] (re-matches #".*(\d)" (second test))
        [_ alt-2] (re-matches #".*(\d)" (last test))
        ]
    {:items (-> items
                (st/split #", ")
                (->> (mapv parse-long)))
     :operation (functions operator)
     :operand (parse-operand operand)
     :test (parse-long divisible-by)
     :alt-1 (parse-long alt-1)
     :alt-2 (parse-long alt-2)
     :inspected 0N}))

(defn inspect [worry-fn {:keys [operation operand]} item]
  (let [result
        (if (= operand :old)
          (worry-fn (operation (bigint item) item))
          (worry-fn (operation (bigint item) operand))
          )]
    (comment (quot (operation (bigint item) item) worry-divided-by)
                   (quot (operation (bigint item) operand) worry-divided-by))
    result))

(defn target [{:keys [test alt-1 alt-2]} item]
  (if (= (rem item test) 0)
    alt-1
    alt-2))

(defn monkey-round [worry-fn {:keys [items] :as monkey}]
  (when monkey
    (let [new-items (map (partial inspect worry-fn monkey) items)
          targets (map (partial target monkey) new-items)]
      [(mapv vector new-items targets)
       (-> monkey
           (assoc :items [])
           (update :inspected + (count items)))])))

(defn update-monkeys [monkeys items]
  (for [index (range (count monkeys))]
    (let [monkey (get monkeys index)
          new-items (map first (filter (fn [[item idx]] (= idx index)) items))]
      (update monkey :items #(apply conj % new-items)))))

(defn round [worry-fn monkeys]
  (loop [index 0
         monkeys monkeys]
    (let [[items monkey] (monkey-round worry-fn (get monkeys index))]
      (if monkey
        (recur (inc index)
               (-> monkeys
                   (assoc index monkey)
                   (update-monkeys items)
                   vec))
        monkeys))))

(defn play [rounds worry-fn monkeys]
  (->> monkeys
       (iterate (partial round worry-fn))
       (take (inc rounds))
       last
       (sort-by :inspected >)
       (take 2)
       (map :inspected)
       (reduce * 1N)))

(def input (mapv parse-monkey (get-input 11 #"\n\n")))

(def pt1 (play 20 #(quot % 3) input))

(def pt2
  (let [denom (reduce * (map :test input))]
    (play 10000 #(rem % denom) input)))
