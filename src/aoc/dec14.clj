(ns aoc.dec14)

(defn str->digits [s]
  (mapv #(- (int %) (int \0)) s))

(defn step [[e1 e2 recipes]]
  (let [score (+ (recipes e1) (recipes e2))
        next-sv (into recipes (str->digits (str score)))]
    [(mod (+ e1 (inc (next-sv e1))) (count next-sv))
     (mod (+ e2 (inc (next-sv e2))) (count next-sv))
     next-sv]))

(defn find-subvec [input steps]
  (loop [idx 0
         [single & more] steps]
    (let [[_ _ recipes] single
          to-check (subvec recipes idx (+ idx 5))]
      (if (= to-check input)
        (dec idx)
        (recur (inc idx) more)))))

(defn run []
  (let [input 77201
        input-seq (str->digits (str input))]
    {:part1 (->> (iterate step [0 1 [3 7]])
                 (drop-while (fn [[_ _ recipes]]
                               (< (count recipes) (+ input 10))))
                 (take 10)
                 (last)
                 (last)
                 (drop input)
                 (take 10)
                 (time))
     :part2 (->> (iterate step [0 1 [3 7]])
                 (drop 3)
                 (find-subvec input-seq)
                 (time))}))
