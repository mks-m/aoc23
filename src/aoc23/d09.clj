(ns aoc23.d09
  (:require [clojure.java.io :as io]))

(defn betweens [nums] (mapv #(- %2 %1) nums (drop 1 nums)))

(defn unroll [nums]
  (loop [[first & rest :as history] (list nums)]
    (if (or (empty? first) (every? zero? first))
      rest
      (recur (conj history (betweens first))))))

(defn rollback [history] (reduce #(+ %1 (last %2)) 0 history))

(def lines (->> "d09" io/resource io/reader line-seq))
(->> lines
     (mapv #(read-string (str "[" % "]")))
     (map unroll)
     (map rollback)
     (reduce +))

(defn rollback-left [history] (reduce #(- (first %2) %1) 0 history))

(->> lines
     (mapv #(read-string (str "[" % "]")))
     (map unroll)
     (map rollback-left)
     (reduce +))