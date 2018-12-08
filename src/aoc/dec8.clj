(ns aoc.dec8
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn md-sum [children mds]
  (reduce + (concat (map :md-sum children) mds)))

(defn get-child-value [children idx]
  (if (zero? idx)
    0
    (:md-sum (get children (dec idx) {:md-sum 0}))))

(defn md-by-index [children mds]
  (reduce + (map #(get-child-value children %) mds)))

(defn read-node-fn [f]
  (fn read-node [[child-cnt md & more]]
    (if (zero? child-cnt)
      {:md-sum (reduce + (take md more))
       :rem    (drop md more)}
      (fn []
        (loop [children []
               rem more
               cnt 0]
          (if (>= cnt child-cnt)
            {:md-sum (f children (take md rem))
             :rem    (drop md rem)}
            (let [child (trampoline read-node rem)]
              (recur
                (conj children child)
                (:rem child)
                (inc cnt)))))))))

(defn run []
  (let [input (as-> (utils/day-file 8) res
                    (first res)
                    (str/split res #" ")
                    (map #(Integer/parseInt %) res))
        part1 (read-node-fn md-sum)
        part2 (read-node-fn md-by-index)]
    {:part1 (-> (trampoline part1 input)
                :md-sum)
     :part2 (-> (trampoline part2 input)
                :md-sum)}))
