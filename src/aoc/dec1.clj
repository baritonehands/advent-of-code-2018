(ns aoc.dec1
  (:require [aoc.utils :as utils]))

(defn sum-str [acc s]
  (+ acc (Integer/parseInt s)))

(defn repeat-sum [xs]
  (loop [repeated? #{}
         sum 0
         [v & rem] (cycle xs)]
    (let [next-sum (sum-str sum v)]
      (if (repeated? next-sum)
        next-sum
        (recur (conj repeated? next-sum) next-sum rem)))))

(defn run []
  (let [input (utils/day-file 1)]
    {:part1 (reduce sum-str 0 input)
     :part2 (repeat-sum input)}))
