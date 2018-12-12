(ns aoc.dec12
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn initial-state [line]
  (.substring line 15))

(defn parse-rule [line]
  (str/split line #" => "))

(defn pad-l [s]
  (apply str (concat (repeat (- 5 (count s)) ".") s)))

(defn pad-r [s]
  (apply str (concat s (repeat (- 5 (count s)) "."))))

(defn next-gen [rules plants]
  (loop [xs plants
         v (apply str (take 5 xs))
         res ["." "."]]
    (if (empty? xs)
      (apply str res)
      (let [rule (get rules v ".")]
        (recur
          (rest xs)
          (apply str (take 5 (rest xs)))
          (conj res rule))))))

(defn sum [xs]
  (->> (for [[idx v] (map vector (range -5 (inc (count xs))) xs)]
         (if (= "#" (str v)) idx 0))
       (reduce +)))

(def test-rules (str/split-lines "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"))
(def test-init ".....#..#.#..##......###...###.....")

(defn strip-gen [gen]
  (let [start (.indexOf gen "#")
        end (.lastIndexOf gen "#")]
    (str "....." (.substring gen start (inc end)) ".....")))

(defn find-cycle [xs]
  (loop [seen #{}
         idx 0
         [gen & more] xs]
    (let [stripped (strip-gen gen)]
      (if (contains? seen stripped)
        [(inc idx) gen]
        (recur
          (conj seen stripped)
          (inc idx)
          more)))))

(defn run []
  (let [[l1 _ & more] (utils/day-file 12)
        init (apply str (concat "....." (seq (initial-state l1)) "....."))
        rules (->> (map parse-rule more)
                   (into {}))
        part-seq (iterate #(next-gen rules %) init)
        [idx repeated] (find-cycle part-seq)]
    {:part1 (->> part-seq
                 (take 21)
                 (last)
                 (sum))
     :part2 (-> (- 50000000000 (dec idx))
                (* 50)
                (+ (sum repeated)))}))
