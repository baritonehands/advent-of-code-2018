(ns aoc.dec9
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse [line]
  (let [words (str/split line #" ")]
    {:players (Integer/parseInt (first words))
     :points  (Integer/parseInt (nth words 6))}))

(defn ll-init [n]
  (let [node (transient {:value n})]
    (assoc! node :next node)
    (assoc! node :prev node)
    node))

(defn ll-insert [{:keys [next] :as root} n]
  (let [node (transient {:value n})]
    (assoc! node :next next :prev root)
    (assoc! next :prev node)
    (assoc! root :next node)
    node))

(defn ll-remove [{:keys [next prev value] :as node}]
  (assoc! next :prev prev)
  (assoc! prev :next next)
  [next (:value node)])

(defn ll-nth [node idx]
  (nth (iterate (if (neg? idx) :prev :next) node) (Math/abs (int idx))))

(defn remove-marble [{:keys [n player cur scores] :as res}]
  (let [[next-cur points] (ll-remove (ll-nth cur -7))
        score (+ (get scores player 0) n points)]
    (-> res
        (assoc :cur next-cur)
        (assoc-in [:scores player] score))))

(defn add-marble [{:keys [n cur] :as res}]
  (assoc res :cur (ll-insert (:next cur) n)))

(defn place-fn [{:keys [players]}]
  (fn [{:keys [n last] :as res}]
    (-> (if (zero? (mod n 23))
          (remove-marble res)
          (add-marble res))
        (update :n inc)
        (update :player #(mod (inc %) players)))))

(defn part [ms points]
  (->> (nth ms (dec points))
       (:scores)
       (sort-by (comp - val))
       (first)))

(defn run []
  (let [{:keys [players points]} (parse (first (utils/day-file 9)))
        ;players 10
        ;points 1618
        place (place-fn {:players players})
        ms (iterate place {:n      1
                           :player 1
                           :cur    (ll-init 0)
                           :scores {}})]
    {:part1 (time (part ms points))
     :part2 (time (part ms (* points 100)))}))
