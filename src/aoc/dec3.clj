(ns aoc.dec3
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn points [[_ x y w h]]
  (set (for [dx (range 0 w)
             dy (range 0 h)]
         [(+ x dx) (+ y dy)])))

(defn ->map [[id x y w h]]
  {:id id :x x :y y :w w :h h})

(def split-re #"#|@|,|:|x")

(defn parse-line [line]
  (->> (str/split line split-re)
       (drop 1)
       (map (comp #(Integer/parseInt %)
                  (memfn trim)))))

(defn intersections [xv]
  (for [idx (range 0 (count xv))
        rhs (subvec xv (inc idx))]
    (set/intersection (xv idx) rhs)))

(defn overlap? [{x1 :x y1 :y w1 :w h1 :h} {x2 :x y2 :y w2 :w h2 :h}]
  (and (< x1 (+ x2 w2))
       (> (+ x1 w1) x2)
       (< y1 (+ y2 h2))
       (> (+ y1 h1) y2)))

(defn non-overlapping [xv]
  (for [idx (range 0 (count xv))
        :when (not-any? #(and (not= (xv idx) %)
                              (overlap? (xv idx) %))
                        xv)]
    (:id (xv idx))))

(defn run []
  (let [input (utils/day-file 3)]
    {:part1 (->> (pmap (comp points parse-line) input)
                 (vec)
                 (intersections)
                 (reduce set/union)
                 (count))
     :part2 (->> (mapv (comp ->map parse-line) input)
                 (non-overlapping)
                 (first))}))
