(ns aoc.dec9
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse [line]
  (let [words (str/split line #" ")]
    {:players (Integer/parseInt (first words))
     :points  (Integer/parseInt (nth words 6))}))

(defn remove-marble [{:keys [marbles n player cur scores] :as res}]
  (let [to-remove (mod (- cur 7) (count marbles))
        *marbles (transient [])
        ;[before after] (split-at to-remove marbles)
        score (+ (get scores player 0) n (marbles to-remove))]
    (reduce conj! *marbles (subvec marbles 0 to-remove))
    (reduce conj! *marbles (subvec marbles (inc to-remove)))
    (-> res
        (assoc :marbles (persistent! *marbles))
        (assoc :cur to-remove)
        (assoc-in [:scores player] score))))

(defn add-marble [{:keys [marbles n cur] :as res}]
  (let [next-idx (mod (+ cur 1) (count marbles))
        *marbles (transient [])]
    (reduce conj! *marbles (subvec marbles 0 (inc next-idx)))
    (conj! *marbles n)
    (reduce conj! *marbles (subvec marbles (inc next-idx)))
    (-> res
        (assoc :marbles (persistent! *marbles))
        (assoc :cur (inc next-idx)))))

(defn place-fn [{:keys [players]}]
  (fn [{:keys [n last marbles] :as res}]
    (if (zero? (mod n 1000))
      (println "N" n "Last" last "Count" (count marbles)))
    (-> (if (zero? (mod n 23))
          (remove-marble res)
          (add-marble res))
        (update :n inc)
        (update :player #(mod (inc %) players)))))

(defn run []
  (let [{:keys [players points]} (parse (first (utils/day-file 9)))
        ;players 10
        ;points 1618
        place (place-fn {:players players})
        output (iterate place {:marbles [0 2 1]
                               :n       3
                               :player  3
                               :cur     1
                               :scores  {}})]
    {:part1 (->> (nth output (- points 3))
                 (:scores)
                 (sort-by val))
     :part2 (->> (nth output (- (* points 100) 3))
                 (:scores)
                 (sort-by val))}))
