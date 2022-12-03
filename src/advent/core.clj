(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn get-input
  ([day split-pattern]
   (let [file (io/resource (str (format "%02d" day) ".txt"))
         input (slurp file)]
     (if split-pattern
       (str/split input split-pattern)
       input)))
  ([day] (get-input day nil)))


