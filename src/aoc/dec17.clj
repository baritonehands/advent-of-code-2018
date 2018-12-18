(ns aoc.dec17
  (:require [aoc.utils :as utils]
            [clojure.string :as str]))

(defn parse-lhs [s]
  (let [[name value] (str/split s #"=")]
    [name (Integer/parseInt value)]))

(defn parse-rhs [s]
  (let [[name value] (str/split s #"=")
        [start end] (str/split value #"\.\.")]
    (for [idx (range (Integer/parseInt start) (inc (Integer/parseInt end)))]
      [name idx])))

(defn parse-line [line]
  (let [[lhs rhs] (str/split line #", ")
        [lname lval] (parse-lhs lhs)]
    (for [[_ rval] (parse-rhs rhs)]
      (if (= lname "x")
        [lval rval]
        [rval lval]))))

(defn print-world [xs ys clay {:keys [water] :as world}]
  (doseq [y ys]
    (println
      (apply str
             (for [x xs]
               (if (clay [x y])
                 \#
                 (or (water [x y]) " "))))))
  world)

(defn adjacent [x y] [[(dec x) y] [x (inc y)] [(inc x) y]])

(defn blocked? [clay pos b]
  (or (boolean (clay pos)) (= \~ b)))

(defn all-blocked? [clay [l d r] [lb db rb]]
  (and (blocked? clay d db)
       (or (blocked? clay l lb) (= lb \|))
       (or (blocked? clay r rb) (= rb \|))))

(defn flow [clay]
  (fn [water]
    (loop [[[[x y] cur] & more] (sort-by (comp vec reverse key) water)
           result water]
      (if (nil? cur)
        result
        (let [[l d r :as adj] (adjacent x y)
              [lb db rb :as blocks] (map result adj)]
          (recur
            more
            (apply
              assoc
              result
              (cond
                (and (= cur \|) (all-blocked? clay adj blocks))
                [[x y] \~]

                (blocked? clay d db)
                (cond-> [[x y] cur]
                        (not (blocked? clay r rb)) (conj r \|)
                        (not (blocked? clay l lb)) (conj l \|))

                (nil? db)
                [[x (inc y)] \|]

                :else
                [[x y] cur]))))))))

(defn next-pos [[[x y] dir]]
  (case dir
    :+y [x (inc y)]
    :-x [(dec x) y]
    :+x [(inc x) y]))

(defn turns [clay water [[x y] dir] [nx ny :as npos]]
  (let [[l d r] (adjacent nx ny)]
    (println l d r npos)
    (cond
      (and (= dir :+y) (blocked? clay d (water d)))
      (cond-> []
              (not (blocked? clay l (water l))) (conj [npos :-x])
              (not (blocked? clay r (water r))) (conj [npos :+x]))

      (= dir :+y)
      nil ; Don't turn

      (not (blocked? clay d (water d)))
      [[npos :+y]]

      :else nil)))

(defn flow2 [clay]
  (fn [{:keys [water src]}]
    (loop [[[_ dir :as cur] & more] src
           next-src []
           next-water water]
      (if-not cur
        {:water next-water
         :src next-src}
        (let [npos (next-pos cur)
              turn-src (doto (turns clay water cur npos) println)]
          (if-not (blocked? clay npos (water npos))
            (recur more
                   (apply conj next-src (or turn-src
                                            [[npos dir]]))
                   (assoc next-water npos \|))
            (recur more
                   (apply conj next-src (or turn-src
                                            [[npos dir]]))
                   (assoc next-water npos \|))))))))

(def test-input (str/split-lines "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"))

(defn run [n]
  (let [clay (set (mapcat parse-line test-input))
        flow-seq (iterate (flow2 clay) {:water {[500 1] \|}
                                        :src [[[500 1] :+y]]})]         ;(utils/day-file 17)))]
    (->> flow-seq
         (take n)
         (map #(doto % println))
         (map #(print-world (range 494 508) (range 0 14) clay %))
         (doall))
    nil))
