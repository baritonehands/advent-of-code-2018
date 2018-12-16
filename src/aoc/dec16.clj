(ns aoc.dec16
  (:require [aoc.utils :as utils]
            [clojure.string :as str]
            [clojure.data :as data]
            [clojure.set :as set]))

(defn str->inst [inst]
  (->> (str/split inst #" ")
       (mapv #(Integer/parseInt %))))

(defn str->state [s]
  (mapv
    #(Integer/parseInt %)
    (-> (.substring s 9 (dec (.length s)))
        (str/split #", "))))

(defn parse-examples [xs]
  (for [[before inst after _] (partition 4 xs)
        :when (.startsWith before "Before:")]
    [(str->state before)
     (str->inst inst)
     (str->state after)]))

(defn arithmeticr [op state a b c]
  (assoc state c (op (state a) (state b))))

(defn arithmetici [op state a b c]
  (assoc state c (op (state a) b)))

(defn addr [state a b c]
  (arithmeticr + state a b c))

(defn addi [state a b c]
  (arithmetici + state a b c))

(defn mulr [state a b c]
  (arithmeticr * state a b c))

(defn muli [state a b c]
  (arithmetici * state a b c))

(defn banr [state a b c]
  (arithmeticr bit-and state a b c))

(defn bani [state a b c]
  (arithmetici bit-and state a b c))

(defn borr [state a b c]
  (arithmeticr bit-or state a b c))

(defn bori [state a b c]
  (arithmetici bit-or state a b c))

(defn setr [state a _ c]
  (assoc state c (state a)))

(defn seti [state a _ c]
  (assoc state c a))

(defn branch [op state a b c]
  (assoc state c (if (op a b) 1 0)))

(defn gtir [state a b c]
  (branch > state a (state b) c))

(defn gtri [state a b c]
  (branch > state (state a) b c))

(defn gtrr [state a b c]
  (branch > state (state a) (state b) c))

(defn eqir [state a b c]
  (branch = state a (state b) c))

(defn eqri [state a b c]
  (branch = state (state a) b c))

(defn eqrr [state a b c]
  (branch = state (state a) (state b) c))

(def ops #{addr addi mulr muli banr bani borr bori
           setr seti gtir gtri gtrr eqir eqri eqrr})

(defn op-set [acc [before [opcode & args] after]]
  (reduce
    (fn [m op]
      (if (= (apply op before args) after)
        (update m opcode (fnil conj #{}) op)
        m))
    acc
    ops))

(defn remove-taken [m taken]
  (into {} (for [[k v] m]
             [k (set/difference v taken)])))

(defn single [m]
  (into {} (filter (comp #(= (count %) 1) val) m)))

(defn find-opcodes [possible]
  (loop [found (single possible)
         [_ rem] (data/diff found possible)]
    (if (= (count found) 16)
      found
      (let [single-rem (single (remove-taken rem (apply set/union (vals found))))
            next-found (merge single-rem found)]
        (recur next-found (data/diff next-found rem))))))

(defn run []
  (let [input (parse-examples (utils/day-file 16 1))
        insts (mapv str->inst (utils/day-file 16 2))
        possible (reduce op-set {} input)
        opcodes (find-opcodes possible)
        perform (fn [state [opcode & args]]
                  (apply (first (opcodes opcode)) state args))]
    {:part1 (->> (for [[before [opcode & args] after] input]
                   (count (for [op ops
                                :when (= (apply op before args) after)]
                            opcode)))
                 (filter #(>= % 3))
                 (count))
     :part2 (reduce
              perform
              [0 0 0 0]
              insts)}))
