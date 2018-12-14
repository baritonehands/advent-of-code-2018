(ns aoc.dec13
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def cart-chars #{\^ \v \> \<})
(def cart-under {\^ \|, \v \|, \> \-, \< \-})

(defn parse-input [input]
  (->> (for [[y line] (map-indexed vector input)
             [x c] (map-indexed vector line)
             :when (not= c \space)]
         [[x y] (if (cart-chars c)
                  [c 0 (cart-under c)]
                  c)])
       (into {})))

(defn print-world [world]
  (for [y (range 0 13)]
    (println
      (apply str
        (for [x (range 0 13)]
          (let [c (world [x y])]
            (if (vector? c) (first c) (or c " "))))))))

(defn carts [world]
  (->> (filter (comp vector? val) world)
       (sort-by (juxt (comp second first) ffirst))))

(defn collisions [world]
  (filter (comp #(= % \X) val) world))

(defn next [[x y] dir]
  (case dir
    \> [(inc x) y]
    \< [(dec x) y]
    \^ [x (dec y)]
    \v [x (inc y)]))

(def l {\> \^ \< \v \^ \< \v \>})
(def r {\> \v \< \^ \^ \> \v \<})

(def turn-idx [l identity r])

(defn intersection [[dir idx]]
  (let [next-dir ((turn-idx idx) dir)]
    [next-dir (mod (inc idx) 3) \+]))

(def turn-mappings
  {[\/ \>] \^
   [\\ \>] \v
   [\/ \<] \v
   [\\ \<] \^
   [\/ \^] \>
   [\\ \^] \<
   [\/ \v] \<
   [\\ \v] \>})

(defn turn [world [x y] [dir idx :as cart]]
  (let [track (world (next [x y] dir))]
    (cond
      (vector? track) \X
      (= track \+) (intersection cart)
      :else [(get turn-mappings [track dir] dir) idx track])))

(defn move [world pos [dir _ under :as cart]]
  (if (= (world pos) \X)
    world ; Already intersected
    (-> world
        (assoc pos under)
        (assoc (next pos dir) (turn world pos cart)))))

(defn move-all [world]
  (reduce
    (fn [next-world [pos cart]]
      (move next-world pos cart))
    world
    (carts world)))

(def test-input (str/split-lines "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/"))

(defn run []
  (let [input (utils/day-file 13)
        world (parse-input input)]
    {:part1 (->> (iterate move-all world)
                 (drop-while (comp empty? collisions))
                 (first)
                 (collisions))}))

