(ns aoc.dec2
  (:require [aoc.utils :as utils]))

(defn match-chars [f lhs rhs]
  (->> (map vector lhs rhs)
       (filter (fn [[x y]] (f x y)))))

(defn match-lines [xs]
  (let [xv (vec xs)]
    (->> (for [idx (range 0 (count xv))
               rhs (subvec xv (inc idx))
               :when (= (count (match-chars not= (xv idx) rhs)) 1)]
           (match-chars = (xv idx) rhs))
         (first)
         (map first)
         (apply str))))

(defn run []
  (let [input (utils/day-file 2)
        part1 (->> (for [line input]
                     (->> (frequencies line)
                          (vals)
                          (filter #{2 3})
                          (set)))
                   (frequencies))
        part2 (match-lines input)]
    {:part1 part1
     :part2 part2}))
