(ns advent.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn get-input [day]
  (let [file (io/resource (str (format "%02d" day) ".txt"))
        input (slurp file)]
    input))


