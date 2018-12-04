(ns aoc.dec4
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(def line-re #"\[\d+-(\d+)-(\d+) (\d+):(\d+)\] (\S+) (\S+)")

(defn to-int [s]
  (Integer/parseInt
    (if (= (.charAt s 0) "0")
      (.substring s 1)
      s)))

(defn parse-line [line]
  (let [[mm dd hour minute op id] (drop 1 (first (re-seq line-re line)))]
    {:month  (to-int mm)
     :day    (to-int dd)
     :hour   (to-int hour)
     :minute (to-int minute)
     :op     (keyword (.toLowerCase op))
     :id     (.substring id 1)}))

(defn track-guards [{:keys [guard start] :as acc} {:keys [op id minute]}]
  (case op
    :guard (assoc acc :guard id)
    :falls (assoc acc :start minute)
    :wakes (update-in acc [:output guard] (fnil conj []) [minute start])))

(defn sleepiest-guard [guards]
  (->> (for [[id ms] guards]
         [id (->> (map #(apply - %) ms)
                  (reduce +))])
       (sort-by (comp - second))
       (ffirst)))

(defn sleepiest-minute [sleeps]
  (->> (mapcat (fn [[end start]] (range start end)) sleeps)
       (frequencies)
       (sort-by val)
       (last)))

(defn run []
  (let [input (utils/day-file 4)
        guards (->> (map parse-line input)
                    (sort-by (juxt :month :day :hour :minute))
                    (reduce track-guards {:output {}})
                    :output)
        guard (sleepiest-guard guards)]
    {:part1 [guard (sleepiest-minute (guards guard))]
     :part2 (->> (for [[id sleeps] guards]
                   [id (sleepiest-minute sleeps)])
                 (sort-by (comp second second))
                 (last))}))
