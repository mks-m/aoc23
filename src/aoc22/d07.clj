(ns aoc22.d07
  (:require [clojure.java.io :as io]))

(def lines-test (->> "d07-test" io/resource io/reader line-seq))

(defn square [x] (* x x))

(def card-rank
  {\A 12 \K 11 \Q 10 \J 9 \T 8 \9 7 \8 6 \7 5 \6 4 \5 3 \4 2 \3 1 \2 0})

(defn parse-hand [hand]
  [(->> hand frequencies vals sort (map square) (reduce +)) 
   (mapv card-rank hand)])

(defn compare-hand [[r1 h1] [r2 h2]]
  (if (= r1 r2) (compare h1 h2) (compare r1 r2)))

(defn parse-line [line]
  (let [[hand bid] (re-seq #"\w+" line)]
    [(parse-hand hand) (Integer/parseInt bid)]))

(def lines (->> "d07" io/resource io/reader line-seq))
(->> lines
     (map parse-line)
     (sort-by first compare-hand)
     (map-indexed #(* (inc %1) (last %2)))
     (reduce +))

;; p2

(def card-rank2
  (into {} (map-indexed #(vector %2 %1) [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A])))

(defn parse-hand2 [hand]
  (let [freqs (frequencies hand)
        jokers (freqs \J 0)
        freqs (dissoc freqs \J)
        [top & tops] (reverse (sort (vals freqs)))]
    [(->> (conj tops (+ top jokers))
          (map square)
          (reduce +))
     (mapv card-rank2 hand)]))

(defn parse-line2 [line]
  (let [[hand bid] (re-seq #"\w+" line)]
    [(parse-hand2 hand) (Integer/parseInt bid)]))

(->> lines
     (map parse-line2)
     (sort-by first compare-hand)
     (map-indexed #(* (inc %1) (last %2)))
     (reduce +))
