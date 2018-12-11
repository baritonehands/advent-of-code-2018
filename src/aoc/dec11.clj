(ns aoc.dec11
  (:require [aoc.utils :as utils]))

(defn hundreds [n]
  (-> (/ n 100.0) int (mod 10)))

(defn level [sn x y]
  (let [rack-id (+ 10 x)]
    (-> (* rack-id y)
        (+ sn)
        (* rack-id)
        (hundreds)
        (- 5))))

(defn powers [sn]
  (->> (for [y (range 0 300)
             x (range 0 300)]
         [[x y] (level sn x y)])
       (into {})))

(defn grid-power [ps n x y]
  (for [dy (range 0 n)
        dx (range 0 n)]
    (ps [(+ x dx) (+ y dy)])))

(defn grid-power-all [ps n]
  (for [y (range 0 (- 300 n))
        x (range 0 (- 300 n))]
    [[[x y] n] (reduce + (grid-power ps n x y))]))

(defn grid-max [[_ ptot :as prev] [_ tot :as cur]]
  (if (>= ptot tot)
    prev
    (doto cur println)))

(defn max-power-all [ps]
  (->> (mapcat #(grid-power-all ps %)
               (range 1 14)) ; Assumed done since no progress after 13
       (reduce grid-max)))

(defn run []
  (let [ps (powers 2187)
        part1 (->> (grid-power-all ps 3)
                   (reduce grid-max))]
    (println "part1:" part1)
    {:part1 part1
     :part2 (max-power-all ps)}))
