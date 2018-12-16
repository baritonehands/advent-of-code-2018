(ns aoc.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn day-file [day]
  (->> (io/resource (str "dec" day ".txt"))
       (slurp)
       (str/split-lines)))

(defn max-by [f xs]
  (apply max (map f xs)))

(defn min-by [f xs]
  (apply min (map f xs)))

(defn ny-distance [[x y] [cx cy]]
  (+ (Math/abs ^int (- cy y))
     (Math/abs ^int (- cx x))))
