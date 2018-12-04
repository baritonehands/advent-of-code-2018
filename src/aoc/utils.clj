(ns aoc.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn day-file [day]
  (->> (io/resource (str "dec" day ".txt"))
       (slurp)
       (str/split-lines)))
