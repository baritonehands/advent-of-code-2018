(ns aoc.dec7
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[[lhs] [rhs]] (->> (str/split line #"tep ")
                           (drop 1))]
    [lhs rhs]))

(defn deps [acc [lhs rhs]]
  (-> acc
      (update-in [:after lhs] (fnil conj #{}) rhs)
      (update-in [:before rhs] (fnil conj #{}) lhs)))

(defn find-order [{:keys [after before]}]
  (loop [executed #{}
         result []
         candidates (set/difference (set (keys after)) (set (keys before)))]
    (if (seq candidates)
      (let [step (-> (filter (fn [c]
                               (every? #(contains? executed %) (get before c #{})))
                             candidates)
                     sort
                     first)
            next-executed (conj executed step)]
        (recur next-executed
               (conj result step)
               (-> (get after step #{})
                   (set/union candidates)
                   (set/difference next-executed))))
      (apply str result))))

(defn run []
  (let [input (utils/day-file 7)]
    {:part1 (->> (map parse-line input)
                 (reduce deps {})
                 (find-order))}))

