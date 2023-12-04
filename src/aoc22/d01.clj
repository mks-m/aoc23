(ns aoc22.d01
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def lines (->> "d01" io/resource io/reader line-seq))

(defn str-to-num [s]
  (let [gs (re-seq #"\d" s)]
    (Integer/parseInt (str (first gs) (last gs)))))

;; p1
(->> lines (map str-to-num) (reduce +))

(def numbers
  {"one" "o1e" "two" "t2o" "three" "t3e" "four" "f4r" "five" "f5e" "six" "s6x"
   "seven" "s7n" "eight" "e8t" "nine" "n9e"})

(def pattern (re-pattern (string/join "|" (keys numbers))))

(defn replace-words [line]
  (loop [line line]
    (let [word (re-find pattern line)]
      (if word
        (recur (string/replace line word (numbers word)))
        line))))

;; p2
(time (->> lines (map replace-words) (map str-to-num) (reduce +)))

;; alt p2

(def numbers2
  {"one" "1" "two" "2" "three" "3" "four" "4" "five" "5" "six" "6"
   "seven" "7" "eight" "8" "nine" "9"})

(def pattern2
  (re-pattern
   (str "(?=(" (string/join "|" (concat (keys numbers2) (range 10))) "))")))

(defn str-to-num2 [s]
  (let [gs (re-seq pattern2 s)
        [[_ fst] [_ lst]] [(first gs) (last gs)]]
    (Integer/parseInt (str (or (numbers2 fst) fst) (or (numbers2 lst) lst)))))

;; p2
(time (->> lines (map str-to-num2) (reduce +)))
