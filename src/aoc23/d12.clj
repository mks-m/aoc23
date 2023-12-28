(ns aoc23.d12 
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def lines (->> "d12-test" io/resource io/reader line-seq))
(defn parse-pattern [pattern]
  (re-pattern (string/replace pattern #"\?" "[.#]")))
(defn parse-line [line]
  (let [[pattern groups] (string/split line #" ")]
    [(parse-pattern pattern) (read-string (str "[" groups "]"))]))
(def parsed (mapv parse-line lines))

(defn fits? [pattern val] (boolean (re-find pattern val)))

;; ?#?#?#?#?#?#?#? 1,3,1,6

;; i1 # i2 ### i3 # i4 ###### i5

(defn empty-injects [groups]
  (vec (repeat (inc (count groups)) 0)))

(empty-injects [1 1 1])

;; ???.### 1,1,3
(re-seq #"\.*[#\?]\.+[#\?]{3}\.+[#\?]\.+[#\?]{6}" "?#?#?#?#?#?#?#?")

(re-seq #"[^$]." "???")
[\? {:current nil :groups [1 1 3]}]

{:current nil :groups [1 1 3]}
