(ns aoc22.d03
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.set :as set]))

(def lines-test (->> "d03-test" io/resource io/reader line-seq))
(defn digit? [char] (#{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} char))
(defn part? [char] (and (not= char \.) (not (digit? char))))

(def initial-state
  {:cur-num nil :cur-rc #{}
   :row 0 :col 0
   :numbers [] :symbols {} :part-rc {}})

(defn add-digit [{:keys [cur-num cur-rc row col] :as state} digit]
  (assoc state
         :cur-num (str cur-num digit)
         :cur-rc (conj cur-rc [row col])))

(defn term-digit [{:keys [cur-num cur-rc numbers] :as state}]
  (assoc state
         :cur-num nil
         :cur-rc #{}
         :numbers (conj numbers [cur-num cur-rc])))

(defn part-rc-for [[r c :as rc]]
  {[(dec r) (dec c)] rc [(dec r)     c ] rc [(dec r) (inc c)] rc
   [     r  (dec c)] rc                     [     r  (inc c)] rc
   [(inc r) (dec c)] rc [(inc r)     c ] rc [(inc r) (inc c)] rc})

(defn add-symbol [{:keys [row col] :as state} next]
  (-> state
      (update :symbols assoc [row col] next)
      (update :part-rc merge (part-rc-for [row col]))))

(defn process-char [state next]
  (cond-> state
    (digit? next) (add-digit next)
    (and (:cur-num state)
         (not (digit? next))) term-digit
    (part? next) (add-symbol next)
    :else (update :col inc)))

(defn process-line [state line]
  (-> (reduce process-char state line)
      (update :row inc)
      (assoc :col 0)))

(defn number-in-parts? [[num rcs] part-rc]
  (let [part-rc-set (set (keys part-rc))
        rc-overlap (set/intersection rcs part-rc-set)]
    (when (not-empty rc-overlap) [num rcs])))

(defn sum-part-nums [lines]
  (let [{:keys [numbers part-rc]}
        (reduce process-line initial-state lines)
        parts (filter #(number-in-parts? % part-rc) numbers)
        part-nums (map #(Integer/parseInt (first %)) parts)]
    (reduce + part-nums)))

;; p1

(def lines (->> "d03" io/resource io/reader line-seq))
(sum-part-nums lines)

(defn part-reach [[rc] numbers]
  (let [gear-reach (set (keys (part-rc-for rc)))]
    (filter #(not-empty (set/intersection gear-reach (last %))) numbers)))

(let [{:keys [symbols numbers]}
      (reduce process-line
              initial-state
              lines)
      stars (filter #(= \* (last %)) symbols)
      reach (map #(part-reach % numbers) stars)
      gears (filter #(= 2 (count %)) reach)
      gear-nums (map (fn [gn] (map #(Integer/parseInt (first %)) gn)) gears)
      gear-ratios (map #(reduce * %) gear-nums)]
  (reduce + gear-ratios))
