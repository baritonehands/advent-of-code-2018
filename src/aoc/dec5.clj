(ns aoc.dec5
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn char-range [c n]
  (let [idx (int c)]
    (map char (range idx (+ idx n)))))

(defn reactive? [a b]
  (or (= (Character/compare a b) 32)
      (= (Character/compare b a) 32)))

(defn react [input]
  (loop [[prev & [c & more :as all]] input
         result []]
    (cond
      (nil? c) (conj result prev)
      (reactive? prev c) (recur more result)
      :else (recur all (conj result prev)))))

(defn step [input]
  (let [next-input (react input)]
    (if (not= input next-input)
      (recur next-input)
      next-input)))

(defn run []
  (let [input (seq (first (utils/day-file 5)))]
    {:part1 (count (step input))
     :part2 (->> (for [[lc uc] (map vector (char-range \a 26) (char-range \A 26))]
                   [lc (-> (remove #{lc uc} input)
                           (step)
                           (count))])
                 (sort-by second)
                 (first))}))
