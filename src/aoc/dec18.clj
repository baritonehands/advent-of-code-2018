(ns aoc.dec18
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse-input [input]
  (into {} (for [[y line] (map-indexed vector input)
                 [x ch] (map-indexed vector line)]
             [[x y] ch])))

(defn print-world [nx ny world]
  (doseq [y (range 0 ny)]
    (println
      (apply str
             (for [x (range 0 nx)]
               (let [c (world [x y])]
                 (or c "."))))))
  (println)
  world)

(defn neighbors [[x y]]
  (for [dx [-1 0 1]
        dy [-1 0 1]
        :when (or (not= dx 0) (not= dy 0))]
    [(+ x dx) (+ y dy)]))

(defn count-type [ch neighbors]
  (count (filter #(= ch %) neighbors)))

(defn step [world]
  (->> (for [[[x y] ch] world]
         (let [counts (->> (neighbors [x y])
                           (map world)
                           (frequencies))
               trees (or (counts \|) 0)
               lumberyards (or (counts \#) 0)]
           [[x y] (case ch
                    \| (if (>= lumberyards 3) \# \|)
                    \# (if (and (>= lumberyards 1) (>= trees 1)) \# \.)
                    (if (>= trees 3) \| \.))]))
       (filter (comp some? second))
       (into {})))

(defn value [world]
  (let [counts (-> world vals frequencies)]
    (* (counts \|) (counts \#))))

(defn part2 [world limit]
  (loop [next-world (step world)
         idx 1
         seen {}]
    (let [total (value next-world)]
      (if (zero? (mod idx 100)) (println idx total))
      (if (> idx limit)
        [(->> (sort-by (juxt (comp count second) (comp - first second)) seen)
              (last)
              (second)
              (first)) total next-world]
        (recur (step next-world) (inc idx) (update seen total (fnil conj []) idx))))))

(def test-input (str/split-lines ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."))

(defn run []
  (let [world (parse-input (utils/day-file 18))
        [idx total start] (part2 world 650)]
    (println idx total)
    {:part1 (->> (iterate step world)
                 (drop 10)
                 (first)
                 (vals)
                 (frequencies))
     :part2 (->> (iterate step start)
                 (take 18)
                 (mapv value))}))
