(ns aoc.dec15
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn parse-input [input]
  (into {} (for [[y line] (map-indexed vector input)
                 [x spot] (map-indexed vector line)
                 :when (not= spot \.)]
             [[x y] (if (#{\G \E} spot)
                      [spot 200]
                      spot)])))

(defn print-world [nx ny world]
  (doseq [y (range 0 (inc ny))]
    (println
      (apply str
             (for [x (range 0 (inc nx))]
               (let [c (world [x y])]
                 (if (vector? c) (first c) (or c " ")))))))
  nil)

(defn units [world]
  (->> (filter (comp vector? val) world)
       (sort-by (comp (juxt second first) key))))

(def opponent {\G \E \E \G})

(defn targets [world ge]
  (->> (units world)
       (filter #(= ge (-> % val first opponent)))))

(defn delta-range [x y]
  (if (< x y) (repeat (- (inc y) x) 1) (repeat (- x (dec y)) -1)))

(defn adjacent [[x y]]
  #{[x (dec y)] [(dec x) y] [(inc x) y] [x (inc y)]})

(defn adjacent-targets [world pos ge]
  (let [adj (adjacent pos)]
    (->> (targets world ge)
         (filter (comp adj first)))))

(defn path-seq [pos ds]
  (drop 1 (reduce
            (fn [acc [dx dy]]
              (let [[x y] (last acc)]
                (conj acc [(+ x dx) (+ y dy)])))
            [pos]
            ds)))

(def reading-order {[0 -1] 0, [-1 0] 1, [1 0] 2, [0 1] 3})

(defn cost [curr start end]
  (let [g (utils/ny-distance start curr)
        h (utils/ny-distance curr end)
        f (+ g h)]
    [f g h]))

(defn a-star [world [x1 y1 :as src] [x2 y2 :as dst]]
  (loop [open [{:pos src :parent nil}]
         closed #{}]
    (let [{:keys [pos parent] :as cur} (first open)
          n-closed (conj closed cur)
          ps (->> (adjacent pos)
                  (remove #(or (world %) (n-closed %))))]
      (loop [n-open (rest open)
             [adj & more] ps]
        (if-not adj
          n-open
          (if-let [found (first (filter #(= (:pos %) adj) n-open))]
            (recur (cons {:pos adj :parent cur} n-open) more)
            (recur (cons {:pos adj :parent cur} n-open) more)))))))

(defn path [world [x1 y1] ts]
  (->> (for [[x2 y2] ts
             delta (combo/permutations
                     (concat
                       (for [dx (drop 1 (delta-range x1 x2))]
                         [dx 0])
                       (for [dy (drop 1 (delta-range y1 y2))]
                         [0 dy])))]
         delta)
       (sort-by (juxt count (comp reading-order first)))
       (map #(path-seq [x1 y1] %))
       (filter #(not-any? world %))
       (ffirst)))

(defn attack [world ats]
  (if-let [[pos [ge hp]] (->> (sort-by (juxt (comp second second)
                                             (comp (juxt second first) first))
                                       ats)
                              (first))]
    (if (<= (- hp 3) 0)
      (dissoc world pos)
      (assoc world pos [ge (- hp 3)]))
    world))

(defn move-one [world [[x y :as pos] [ge _ :as unit]]]
  (if-let [ats (seq (adjacent-targets world pos ge))]
    (attack world ats)
    (let [ts (targets world ge)]
      (if-let [next-pos (path world pos (mapcat (comp adjacent first) ts))]
        (let [next-world (-> world
                             (assoc next-pos unit)
                             (dissoc pos))]
          (attack next-world (adjacent-targets next-world next-pos ge)))
        world))))

(defn move-all [world]
  (reduce move-one world (units world)))

(def test-input (str/split-lines "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########"))
(def test-input2 ["#######" "#.G...#" "#...EG#" "#.#.#G#" "#..G#E#" "#.....#" "#######"])

(defn run []
  (let [world (parse-input test-input2)]                     ;(utils/day-file 15))]
    (->> (iterate move-all world)
         (drop 23)
         (first)
         (print-world 8 8))))
