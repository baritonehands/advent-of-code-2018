(ns aoc.dec19
  (:require [aoc.utils :as utils]
            [aoc.dec16 :as dec16]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [[opcode & args] (str/split line #" ")
        f @(resolve (symbol "aoc.dec16" opcode))
        [a b c] (map #(Integer/parseInt %) args)]
    (fn [state]
      (f state a b c))))

(defn parse-input [input]
  (let [[ip & lines] input]
    {:ip-register  (Integer/parseInt (.substring ip 4))
     :instructions (mapv parse-line lines)}))

(defn can-skip? [prev cur]
  (let [[[_ _ l] [_ _ r] b] (clojure.data/diff prev cur)]
    (if (= (count (filter nil? b)) 1)
      (let [delta (- r l)
            mult (int (/ (- 10551361 r) delta))]
        (assoc cur 2 (+ r (* mult delta)))))))


(defn execute [ip state {:keys [ip-register instructions]}]
  (loop [ip ip
         state state
         idx 0
         rep state]
    (when (= (state 2) 10551360);(zero? (mod idx 100000))
      (println idx ip state rep))
    (if (>= ip (count instructions))
      state
      (if (and (zero? (mod idx 10000))
               (not= rep state)
               (can-skip? rep state))
        (let [skip-to (can-skip? rep state)]
          (recur ip skip-to (inc idx) skip-to))
        (let [f (instructions ip)
              op-state (assoc state ip-register ip)
              next-state (f op-state)]
          (recur
            (inc (next-state ip-register))
            next-state
            (inc idx)
            (if (zero? (mod idx 10000))
              state
              rep)))))))

(def test-input (str/split-lines "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"))

(defn run []
  (let [input (parse-input (utils/day-file 19))
        furthest [1 36936 2129 2 10551361 0]
        furthest2 [157484 2466313 1671 5 10551361 0]]
    {:part1 (execute 0 [0 0 0 0 0 0] input)}))
     ;:part2 (execute (inc (furthest2 3)) furthest2 input)}))
