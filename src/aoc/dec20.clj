(ns aoc.dec20
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-input [input]
  (-> input
      (str/replace "^" "[")
      (str/replace "$" "]")
      (str/replace "|" " | ")
      (read-string)))

(defn print-world [world doors]
  (let [xmin (utils/min-by first world)
        xmax (utils/max-by first world)
        ymin (utils/min-by second world)
        ymax (utils/max-by second world)
        border (fn [y]
                 (println
                   (str
                     (apply str
                            (reduce #(conj %1 (if (doors [%2 [y (inc y)]]) "#-" "##")) [] (range xmin (inc xmax))))
                     "#")))]
    (border (dec ymin))
    (doseq [y (range ymin (inc ymax))]
      (println
        (apply str
               "#"
               (for [x (range xmin (inc xmax))]
                 (str
                   (cond
                     (= [x y] [0 0]) "X"
                     (world [x y]) "."
                     :else "#")
                   (if (doors [[x (inc x)] y])
                     "|"
                     "#")))))
      (border y))))

(defn move [[x y] dir]
  (case dir
    \N [x (dec y)]
    \S [x (inc y)]
    \E [(inc x) y]
    \W [(dec x) y]))

(defn door [[x1 y1] [x2 y2]]
  (if (= x1 x2)
    [x1 (sort [y1 y2])]
    [(sort [x1 x2]) y1]))

(defn path-step [{:keys [pos result doors]} dir]
  (let [next-pos (move pos dir)]
    {:pos    next-pos
     :result (conj result next-pos)
     :doors (conj doors (door pos next-pos))}))

(defn path [pos result doors xs]
  (reduce path-step {:pos    pos
                     :result result
                     :doors doors} xs))

(defn or? [x]
  (and (list? x) (some #(= % '|) x)))

(defn parse-or [xs]
  (let [next-xs (->> (partition-by #(= % '|) xs)
                     (remove #(= % '(|))))]
    (if (= (last xs) '(|))
      (concat next-xs [nil])
      next-xs)))

(defn union2 [[xs ys] [xs2 ys2]]
  [(set/union xs xs2) (set/union ys ys2)])

(defn trace-path [pos re]
  (loop [pos pos
         [cur & more] re
         result #{pos}
         doors #{}]
    ;(println pos cur result doors)
    (if-not cur
      [result doors]
      (if (or? cur)
        (let [[next-result next-doors] (union2 [result doors]
                                               (->> (map #(trace-path pos %) (parse-or cur))
                                                    (reduce union2)))]
          (recur pos more next-result next-doors))
        (let [{next-result :result
               next-doors  :doors
               next-pos    :pos} (path pos result doors (name cur))]
          (recur next-pos more next-result next-doors))))))

(def test-input "^ENWWW(NEEE|SSE(EE|N))$")
(def test-input2 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$\n")
(def test-input3 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")

(defn run []
  (let [input (parse-input (first (utils/day-file 20)))]
    (trace-path [0 0] input)))
