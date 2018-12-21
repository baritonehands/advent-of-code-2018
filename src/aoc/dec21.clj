(ns aoc.dec21
  (:require [aoc.utils :as utils]
            [aoc.dec19 :as dec19]))

(defn execute [ip state {:keys [ip-register instructions]} limit]
  (loop [ip ip
         state state
         idx 0
         seen (sorted-set)]
    ;(if (zero? (mod idx 1000))
    ;  (println idx state seen))
    (if (or (and (pos? limit) (>= (count seen) limit))
            ;(seen (state 4))
            (>= ip (count instructions)))
      [idx seen]
      (let [f (instructions ip)
            op-state (assoc state ip-register ip)
            next-state (f op-state)]
        (recur
          (inc (next-state ip-register))
          next-state
          (inc idx)
          (if (= (op-state ip-register) 28)
            (do
              ;(println (inc (count seen)))
              (conj seen (state 1)))
            seen))))))

(defn part1 [input]
  (let [[_ seen] (execute 0 [0 0 0 0 0 0] input 99)]
    (->> (for [x seen]
           (let [[idx] (execute 0 [x 0 0 0 0 0] input -1)]
             (println "Result" x idx)
             [idx x]))
         (sort))))

(defn run []
  (let [input (dec19/parse-input (utils/day-file 21))
        [_ seen] (execute 0 [0 0 0 0 0 0] input 1000)]
    ;(part1 input)
    (->> (filter #(> 173927864 % 12163489) seen)
         (pmap
           (fn [x]
             (let [[idx] (execute 0 [x 0 0 0 0 0] input -1)]
               (println "Result" x idx)
               [idx x])))
         (sort))))



