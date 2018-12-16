(ns aoc.dec6
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse-line [line]
  (->> (str/split line #",\s+")
       (mapv #(Integer/parseInt %))))

(defn all-distances [coords lhs]
  (let [shortest (->> (map-indexed (fn [idx coord]
                                     [idx (utils/ny-distance lhs coord)]) coords)
                      (sort-by second)
                      (partition-by second)
                      (first))]
    (if (> (count shortest) 1)
      -1
      (ffirst shortest))))

(defn lookup [coords [idx cnt]]
  (if (pos? idx)
    [(coords idx) cnt]))

(defn part1 [coords xmax ymax]
  (->> (for [x (range 0 xmax)
             y (range 0 ymax)]
         [x y])
       (map #(all-distances coords %))
       (frequencies)
       (sort-by val)
       (map #(lookup coords %))))

(defn part2 [coords xmin ymin xmax ymax]
  (->> (for [x (range xmin xmax)
             y (range ymin ymax)
             :when (< (reduce + (map #(utils/ny-distance [x y] %) coords)) 10000)]
         true)
       (count)))

(defn run []
  (let [input (utils/day-file 6)]
    (let [coords (mapv parse-line input)
          xmin (apply min (map first coords))
          ymin (apply min (map second coords))
          xmax (apply max (map first coords))
          ymax (apply max (map second coords))]
      {:part1 (part1 coords xmax ymax)
       :part2 (part2 coords xmin ymin xmax ymax)})))

