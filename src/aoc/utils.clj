(ns aoc.utils
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn day-file
  ([day] (day-file day nil))
  ([day part]
   (->> (io/resource (str "dec" day (if part (str "-" part)) ".txt"))
        (slurp)
        (str/split-lines))))

(defn max-by [f xs]
  (apply max (map f xs)))

(defn min-by [f xs]
  (apply min (map f xs)))

(defn ny-distance [[x y] [cx cy]]
  (+ (Math/abs ^int (- cy y))
     (Math/abs ^int (- cx x))))
