(ns aoc.dec10
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[_ x y _ vx vy] (->> (str/split line #"<|>|,")
                             (map (memfn trim)))]
    (mapv #(Integer/parseInt %) [x y vx vy])))

(defn maxes [ps]
  (let [xmin (utils/min-by first ps)
        ymin (utils/min-by second ps)
        xmax (inc (utils/max-by first ps))
        ymax (inc (utils/max-by second ps))]
    [[xmin ymin] [xmax ymax]]))

(defn chart [ps]
  (let [[[xmin ymin] [xmax ymax]] (maxes ps)
        points (->> (map #(subvec % 0 2) ps)
                    (set))]
    (doseq [y (range ymin ymax)]
      (println
        (apply str (for [x (range xmin xmax)]
                     (if (points [x y]) "X" ".")))))))

(defn move [ps]
  (for [[x y vx vy] ps]
    [(+ x vx) (+ y vy) vx vy]))

(defn msg-step [{:keys [idx bounds points]}]
  (let [next-points (move points)]
    {:idx    (inc idx)
     :bounds (maxes next-points)
     :points next-points
     :last   bounds}))

(defn find-msg [input]
  (let [maxes-seq (iterate msg-step {:idx    0
                                     :bounds [[Integer/MIN_VALUE Integer/MIN_VALUE]
                                              [Integer/MAX_VALUE Integer/MAX_VALUE]]
                                     :points input})]
    (last (take-while
            (fn [{:keys [bounds last]}]
              (or
                (not last)
                (neg? (compare last bounds))))
            maxes-seq))))

(defn run []
  (let [input (->> (utils/day-file 10)
                   (mapv parse-line))
        res (find-msg input)]

    {:part1 (chart (:points res))
     :part2 (:idx res)}))
