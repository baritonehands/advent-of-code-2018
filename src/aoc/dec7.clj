(ns aoc.dec7
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-line [line]
  (let [[[lhs] [rhs]] (->> (str/split line #"tep ")
                           (drop 1))]
    [lhs rhs]))

(defn add-deps [acc [lhs rhs]]
  (-> acc
      (update-in [:downstream lhs] (fnil conj #{}) rhs)
      (update-in [:upstream rhs] (fnil conj #{}) lhs)))

(defn initial-candidates [{:keys [downstream upstream]}]
  (set/difference (set (keys downstream)) (set (keys upstream))))

(defn next-candidates [done upstream candidates]
  (sort (filter (fn [c]
                  (every? #(contains? done %) (get upstream c #{})))
                candidates)))

(defn find-order [{:keys [downstream upstream] :as deps}]
  (loop [done #{}
         result []
         candidates (initial-candidates deps)]
    (if-let [step (first (next-candidates done upstream candidates))]
      (let [next-executed (conj done step)]
        (recur next-executed
               (conj result step)
               (-> (get downstream step #{})
                   (set/union candidates)
                   (set/difference next-executed))))
      (apply str result))))

(defn char-duration [c]
  (+ 61 (- (int c) (int \A))))

(defn remove-done-working [done workers steps]
  (let [worker-keys (set (map first workers))]
    (remove #(or (contains? worker-keys %)
                 (contains? done %)) steps)))

(defn find-duration [{:keys [downstream upstream] :as deps}]
  (loop [sec -1
         workers #{}
         done #{}
         result []
         candidates (initial-candidates deps)]
    (if (or (seq candidates)
            (seq workers))
      (let [done-now (->> (filter #(<= (second %) sec) workers)
                          (map first))
            next-done (into done done-now)
            next-result (into result done-now)
            workers-now (set (remove #(<= (second %) sec) workers))
            avail (- 5 (count workers-now))
            steps (->> (next-candidates next-done upstream candidates)
                       (remove-done-working next-done workers-now)
                       (take avail))
            next-workers (into workers-now (map #(vector % (+ sec (char-duration %))) steps))]

        (recur (inc sec)
               next-workers
               next-done
               next-result
               (-> (reduce set/union (map #(get downstream % #{}) steps))
                   (set/union candidates)
                   (set/difference next-done))))
      sec)))

(defn run []
  (let [input (utils/day-file 7)
        deps (->> (map parse-line input)
                  (reduce add-deps {}))]
    {:part1 (find-order deps)
     :part2 (find-duration deps)}))
