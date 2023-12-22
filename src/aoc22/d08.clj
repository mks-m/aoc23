(ns aoc22.d08
  (:require [clojure.java.io :as io]))

(def lines (->> "d08-test" io/resource io/reader line-seq))
(def lines (->> "d08" io/resource io/reader line-seq))

(def rl (->> lines first seq (mapv identity)))
(def maze (reduce (fn [maze line]
                    (let [[src dl dr] (re-seq #"\w+" line)]
                      (assoc maze src {\L dl \R dr})))
                  {} (drop 2 lines)))

(reduce (fn [[room steps] turn]
          (if (= "ZZZ" room)
            (reduced steps)
            [((maze room) turn) (inc steps)])) ["AAA" 0] (cycle rl))

;; p2

(defn step-room [[[_ _ x :as room] steps] turn]
  (if (= \Z x) (reduced steps) [((maze room) turn) (inc steps)]))

(def start (filterv #(= \A (nth % 2)) (keys maze)))

(defn gcd [a b] (if (zero? b) a (recur b, (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))
(defn lcmv [& v] (reduce lcm v))

(apply lcmv (map #(reduce step-room [% 0] (cycle rl)) start))
