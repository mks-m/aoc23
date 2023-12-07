(ns aoc22.d06
  (:require [clojure.java.io :as io]))

(def lines-test ["Time:      7  15   30"
                 "Distance:  9  40  200"])

(def times-test (->> lines-test first (re-seq #"\d+") (mapv #(Integer/parseInt %))))
(def dists-test (->> lines-test second (re-seq #"\d+") (mapv #(Integer/parseInt %))))

(defn wins [time best]
  (loop [hold 0
         wins 0]
    (if (>= time hold)
      (recur (inc hold)
             (let [dist (* hold (- time hold))]
               (if (> dist best) (inc wins) wins)))
      wins)))

(def lines ["Time:        56     97     77     93"
            "Distance:   499   2210   1097   1440"])

(def times (->> lines first (re-seq #"\d+") (mapv #(Integer/parseInt %))))
(def dists (->> lines second (re-seq #"\d+") (mapv #(Integer/parseInt %))))

(->> [times dists]
     (apply map wins)
     (reduce *))

(wins 56977793 499221010971440)
