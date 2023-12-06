(ns aoc22.d04
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

#_(def lines-test (->> "d04-test" io/resource io/reader line-seq))

(defn parse-card [line]
  (let [[[_ card-id win-str mine-str]] (re-seq #"^Card +(\d+):(.*)\|(.*)$" line)
        win (set (read-string (str "[" win-str "]")))
        mine (set (read-string (str "[" mine-str "]")))]
    [(Integer/parseInt card-id) win mine]))

(defn card-points [[_ win mine :as card]]
  (conj card (int (Math/pow 2 (dec (count (set/intersection win mine)))))))

(def lines (->> "d04" io/resource io/reader line-seq))

(->> lines
     (map parse-card)
     (map card-points)
     (map last)
     (reduce +))

(defn play-card [[origs copies] [id win mine]]
  (let [origs       (conj origs id)
        num-current  (inc (copies id 0))
        ids-to-copy (map #(+ id % 1)
                         (range (count (set/intersection win mine))))
        copies      (reduce #(update %1 %2 (fnil + 0) num-current)
                            copies
                            ids-to-copy)]
    [origs copies]))

(let [[origs copies] (->> lines
                          (map parse-card)
                          (reduce play-card [[] {}]))]
  (->> (map #(copies % 0) origs)
       (reduce + (count origs))))
