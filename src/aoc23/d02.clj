(ns aoc23.d02
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def limits {"red" 12 "green" 13 "blue" 14})
(def lines (->> "d02" io/resource io/reader line-seq))

(defn parse-draw [draw]
  (let [groups (re-seq #"((\d+) (red|blue|green))+" draw)]
    (reduce (fn [h g] (let [[_ _ n c] g] (assoc h c (Integer/parseInt n))))
            {} groups)))

(defn parse-game [line]
  (let [[_ gid gdata] (re-matches #"Game (\d+): (.*)" line)
        groups (->> (string/split gdata #";") (map parse-draw))]
    [(Integer/parseInt gid) groups]))

(defn impossible-map? [hm] (not-empty (filter #(> (last %) (limits (first %))) hm)))
(defn impossible-set? [ms] (not-empty (filter impossible-map? ms)))
(defn possible-gid [[gid game]] (if (not-empty (impossible-set? game)) 0 gid))

;; p1
(->> lines (map parse-game) (map possible-gid) (reduce +))

(defn merge-min [min-set new-set]
  (reduce (fn [ms [nk nv]] (if (> nv (ms nk 0)) (assoc ms nk nv) ms)) 
          min-set new-set))

(defn power [gs] (reduce * (vals gs)))

;; p2
(->> lines (map parse-game) (map last)
     (map #(reduce merge-min {} %))
     (map power) (reduce +))
