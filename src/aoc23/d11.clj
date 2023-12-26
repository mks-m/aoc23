(ns aoc23.d11 
  (:require [clojure.java.io :as io]))

(def lines (->> "d11" io/resource io/reader line-seq))
(defn parse-line [line] (mapv {\. {:v 1 :h 1} \# {:g true}} line))
(def space-metric (mapv parse-line lines))
(def rows (count space-metric))
(def cols (count (first space-metric)))
(defn rc [[r c]] (get-in space-metric [r c]))
(defn empty-row? [r] (->> r space-metric (filter :g) empty?))
(defn empty-col? [c] (->> cols range (map #(rc [% c])) (filter :g) empty?))
(def empty-rows (set (filter empty-row? (range rows))))
(def empty-cols (set (filter empty-col? (range cols))))
(def galaxy-rcs 
  (->> (for [r (range rows) c (range cols)] [r c])
       (reduce #(if (:g (rc %2)) (conj %1 %2) %1) [])))
(def galaxy-pairs
  (mapcat identity
          (for [g1 (range (count galaxy-rcs))]
            (for [g2 (range (inc g1) (count galaxy-rcs))]
              [g1 g2]))))
(defn galaxy-dist [[r1 c1] [r2 c2]]
  (let [rempty (count (filter empty-rows (range r1 r2 (compare r2 r1)))) 
        cempty (count (filter empty-cols (range c1 c2 (compare c2 c1))))] 
    (+ (* (- r2 r1) (compare r2 r1)) rempty
       (* (- c2 c1) (compare c2 c1)) cempty)))

(reduce #(+ %1 (apply galaxy-dist (map galaxy-rcs %2))) 0 galaxy-pairs)

;; p2

(defn galaxy-dist2 [[r1 c1] [r2 c2]]
  (let [[rmin rmax] (sort [r1 r2])
        [cmin cmax] (sort [c1 c2])
        rempty (count (filter empty-rows (range rmin rmax)))
        cempty (count (filter empty-cols (range cmin cmax)))]
    (+ (- rmax rmin) (* 999999 rempty)
       (- cmax cmin) (* 999999 cempty))))

(reduce #(+ %1 (apply galaxy-dist2 (map galaxy-rcs %2))) 0 galaxy-pairs)
