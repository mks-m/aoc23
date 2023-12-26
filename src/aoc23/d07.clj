(ns aoc23.d07
  (:require [clojure.java.io :as io]))

(def lines-test (->> "d07-test" io/resource io/reader line-seq))

(def card-rank
  (into {} (map-indexed #(vector %2 %1)
                        [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A])))

(defn parse-hand [hand]
  [(->> hand frequencies vals sort (map #(* % %)) (reduce +)) 
   (mapv card-rank hand)])

(defn compare-hand [[r1 h1] [r2 h2]]
  (if (= r1 r2) (compare h1 h2) (compare r1 r2)))

(defn parse-line [line]
  (map #(%1 %2) [parse-hand read-string] (re-seq #"\w+" line)))

(def lines (->> "d07" io/resource io/reader line-seq))

(->> lines
     (map parse-line)
     (sort-by first compare-hand)
     (map-indexed #(* (inc %1) (last %2)))
     (reduce +))

;; p2

(def card-rank2
  (into {} (map-indexed #(vector %2 %1)
                        [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A])))

(defn parse-hand2 [hand]
  (let [freqs (frequencies hand)
        jokers (freqs \J 0)
        [top & tops] (-> (dissoc freqs \J) vals sort reverse)]
    [(->> (conj tops (+ (or top 0) jokers)) (map #(* % %)) (reduce +))
     (mapv card-rank2 hand)]))

(defn parse-line2 [line]
  (map #(%1 %2) [parse-hand2 read-string] (re-seq #"\w+" line)))

(->> lines
     (map parse-line2)
     (sort-by first compare-hand)
     (map-indexed #(* (inc %1) (last %2)))
     (reduce +))
