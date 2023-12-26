(ns aoc23.d05
  (:require [clojure.java.io :as io]))

(add-tap println)

(def lines (->> "d05" io/resource io/reader line-seq))

(def chain
  [:soil :fertilizer :water :light :temperature :humidity :location])

(defn parse-seeds [line]
  (->> line (re-seq #"\d+") (map read-string) (partition 2)
       (map (fn [[s n]] [s (+ s n -1)]))))

(defn parse-map [{:keys [dst] :as garden} line]
  (let [[ds ss rng] (map #(read-string %) (re-seq #"\d+" line))]
    (update-in garden [:maps dst] conj [ss (+ ss rng -1) (- ds ss)])))

(defn parse-lines [garden line] 
  (cond (re-find #"^seeds:" line) (assoc garden :seeds (parse-seeds line))
        (re-find #"-to-" line)    (let [[[_ src dst]] (re-seq #"(\w+)-to-(\w+)" line)]
                                    (assoc garden :src (keyword src) :dst (keyword dst)))
        (re-find #"^\d" line)     (parse-map garden line)
        :else                     garden))

(def garden (reduce parse-lines {} lines))

(defn split-offset [[s1 e1] [s2 e2 o]]
  (cond (or (< e1 s2) (< e2 s1))    [[] [[s1 e1]]]
        (and (>= s1 s2) (>= e2 e1)) [[[(+ s1 o) (+ e1 o)]] []]
        (and (< s1 s2) (< e2 e1))   [[[(+ s2 o) (+ e2 o)]]
                                     [[s1 (max s1 (dec s2))]
                                      [(min e1 (inc e2)) e1]]]
        (< s2 s1)                   [[[(+ s1 o) (+ e2 o)]] 
                                     [[(min e1 (inc e2)) e1]]]
        :else                       [[[(+ s2 o) (+ e1 o)]]
                                     [[s1 (max s1 (dec s2))]]]))

(defn split-reducer [[unmapped mapped] mapping]
  (if (not-empty unmapped)
    (let [[mapped' unmapped'] (split-offset (first unmapped) mapping)]
      [(into (rest unmapped) unmapped') (into mapped mapped')])
    (reduced [[] mapped])))

(defn map-seed-ranges [seed-range-coll mapping]
  (apply into (reduce split-reducer [seed-range-coll []] mapping)))

(time (let [maps-in-order (mapv (:maps garden) chain)
            seed-ranges (:seeds garden)]
        (->> (reduce map-seed-ranges seed-ranges maps-in-order)
             (map first)
             (apply min))))
