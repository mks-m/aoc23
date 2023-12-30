(ns aoc23.d12 
  (:require [clojure.java.io :as io]
            [clojure.string :as string :refer [join]]))

(def lines (->> "d12" io/resource io/reader line-seq))

(defn parse-line [line]
  (let [[pattern groups] (string/split line #" ")]
    [pattern (read-string (str "[" groups "]"))]))
(def parsed (mapv parse-line lines))
(defn empty-injects [gs] (concat [0] (repeat (dec (count gs)) 1) [0]))

(defn parse-line2 [line]
  (let [[pattern groups] (string/split line #" ")]
    [(join "?" (repeat 5 pattern))
     (read-string (str "[" (join "," (repeat 5 groups)) "]"))]))

(def parsed2 (mapv parse-line2 lines))

(defn fits? [ps ts]
  (let [tl (count ts)]
    (loop [idx 0]
      (if (>= idx tl)
        true
        (if (or (= \? (nth ps idx))
                (= (nth ps idx) (nth ts idx)))
          (recur (inc idx))
          false)))))

(defn dist [[x m] k]
  (if-let [x' (m k)]
    [(+ x x') m]
    (let [[p [g & gr] [i & ir] r] k]
      (if (nil? g)
        [(+ x (if (fits? p (.repeat "." (+ i r))) 1 0)) m]
        (let [exp (loop [d 0 args []]
                    (if (> d r)
                      args
                      (recur (inc d)
                             (let [t (str (.repeat "." (+ i d)) g)]
                               (if (fits? p t)
                                 (conj args [(subs p (count t)) gr ir (- r d)])
                                 args)))))
              [x' m'] (reduce dist [0 m] exp)]
          [(+ x x') (assoc m' k x')])))))

(defn arrs [[pattern groups]]
  (let [injects (empty-injects groups)
        to-inject (- (count pattern) (reduce + groups) (dec (count groups)))]
    (first (dist [0 {}]
                 [pattern
                  (mapv #(apply str (repeat % \#)) groups)
                  injects
                  to-inject]))))

(->> parsed (map arrs) (reduce +))
(time (->> parsed2 (map arrs) (reduce +)))
