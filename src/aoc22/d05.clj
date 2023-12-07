(ns aoc22.d05
  (:require [clojure.java.io :as io]))

(add-tap println)

(def lines-test (->> "d05-test" io/resource io/reader line-seq))

(def chain
  [:soil :fertilizer :water :light :temperature :humidity :location])

(defn parse-seeds [line]
  (->> line (re-seq #"\d+") (map #(read-string %))))

(defn parse-lines [{:keys [dst parse-seeds] :as garden} line] 
  (cond
    (re-find #"^seeds:" line)
    (assoc garden :seeds (parse-seeds line))

    (re-find #"-to-" line)
    (let [[[_ src dst]] (re-seq #"(\w+)-to-(\w+)" line)]
      (assoc garden
             :src (keyword src)
             :dst (keyword dst)))

    (re-find #"^\d" line)
    (let [[dst-start src-start rng] (map #(read-string %)
                                         (re-seq #"\d+" line))
          src-end (+ src-start rng -1)
          src-offset (- dst-start src-start)]
      (update-in garden [:maps dst]
                 conj [src-start src-end src-offset]))

    :else garden))

(defn apply-mapping [mapping idx]
  (reduce (fn [idx [ms me mo]]
            (if (<= ms idx me) (reduced (+ idx mo)) idx))
          idx
          mapping))

(defn chain-walk [apply-mapping maps seed]
  (reduce #(apply-mapping (maps %2) %1) seed chain))

(def lines (->> "d05" io/resource io/reader line-seq))

(let [{:keys [seeds maps]} (reduce parse-lines
                                   {:parse-seeds parse-seeds}
                                   lines)]
  (apply min (mapv #(chain-walk apply-mapping maps %) seeds)))

;; p2 naive

(defn rrange [[start num]] (range start (+ start num)))

(defn parse-seed-ranges [line]
  (->> line (re-seq #"\d+") (map read-string) (partition 2)))

(defn range-walk [apply-mapping maps seed-range]
  (future (let [_ (tap> ["starting" seed-range])
                result (time (apply min (map #(chain-walk apply-mapping maps %)
                                             (rrange seed-range))))
                _ (tap> ["finished" seed-range result])]
            result)))

(time
 (let [{:keys [seed-ranges maps]} 
       (reduce parse-lines {:parse-seeds parse-seed-ranges} lines)]
   (apply min (->> seed-ranges
                   (mapv #(range-walk apply-mapping maps %))
                   (map deref)))))

;; p2 sorted maps

(defn sort-mapping-by-gap [[k m]] [k (sort-by #(- (nth % 1) (nth % 0)) m)])

(time
 (let [{:keys [seed-ranges maps]}
       (reduce parse-lines {:parse-seeds parse-seed-ranges} lines)
       sorted-maps (into {} (map sort-mapping-by-gap maps))]
   (apply min (->> seed-ranges
                   (mapv #(range-walk apply-mapping sorted-maps %))
                   (map deref)))))

;; p2 map tree

(defn sort-mapping-asc [[k m]] [k (sort-by first m)])

(defn mapping-tree [segments]
  (let [[left right] (split-at (/ (count segments) 2) segments)
        mid (last left)
        left (not-empty (drop-last 1 left))
        right (not-empty right)]
    [mid (when left (mapping-tree left)) (when right (mapping-tree right))]))

(defn lookup-mtree [[[start end offset] left right] val]
  (cond
    (< val start) (if left (lookup-mtree left val) val)
    (> val end) (if right (lookup-mtree right val) val)
    :else (+ val offset)))

(time
 (let [{:keys [seeds maps]}
       (reduce parse-lines {:parse-seeds parse-seed-ranges} lines)
       mtrees (->> maps
                   (map sort-mapping-asc)
                   (map (fn [[k m]] [k (mapping-tree m)]))
                   (into {}))]
   (apply min (->> seeds
                   (mapv #(range-walk lookup-mtree mtrees %))
                   (map deref)))))
