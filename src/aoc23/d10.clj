(ns aoc23.d10
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(def lines (->> "d10" io/resource io/reader line-seq))

(def conn
  ;; symbol -> [north south west east]
  {\F [false true false true]
   \7 [false true true false]
   \J [true false true false]
   \L [true false false true]
   \- [false false true true]
   \| [true true false false]
   \. [false false false false]
   \O [false false false false]
   \S [true true true true]})

(def rows (count lines))
(def cols (count (first lines)))
(defn idx [[r c]]
  (when (and (< -1 r rows) (< -1 c cols))
    (+ c (* r cols))))
(defn rc [idx] [(quot idx cols) (rem idx cols)])

(def nodes-raw
  (->> lines (map seq) (mapcat identity) (into [])))

(def nodes-parsed
  (->> nodes-raw (map conn) (map-indexed #(conj %2 %1)) (into [])))

(defn conns [[n s w e idx']]
  (let [[r c] (rc idx')
        conns (cond-> []
                n (conj (idx [(dec r) c]))
                s (conj (idx [(inc r) c]))
                w (conj (idx [r (dec c)]))
                e (conj (idx [r (inc c)])))]
    (into #{} (filter identity conns))))

(def nodes-connected (->> nodes-parsed (mapv conns)))

(defn step [path current]
  (let [prev (peek path)
        [c1 c2] (seq (nth nodes-connected current))
        next (if (= prev c1) c2 c1)
        path' (conj path current)]
    (cond
      (not next) [path' false]
      (= next (first path)) [path' true]
      (< -1 (.indexOf path next)) [path' true :lol]
      :else [path' next])))

(defn walk-direction [start-idx dir]
  (loop [path [start-idx] cur dir]
    (let [[path' cur' opt] (step path cur)]
      (if (boolean? cur')
        [path' cur' opt]
        (recur path' cur')))))

(def start-idx
  (->> nodes-raw
       (map-indexed vector)
       (filter #(= \S (second %)))
       first first))

(def start-directions
  (->> start-idx
       nodes-connected
       (filter #((nodes-connected %) start-idx))))

(def start-direction (last start-directions))

(def loop-nodes
  (->> (walk-direction start-idx start-direction) first))

(def loop-set (into #{} loop-nodes))

(def dn [-1 0])
(def ds [1 0])
(def dw [0 -1])
(def de [0 1])
(def dnw [-1 -1])
(def dne [-1 1])
(def dsw [1 -1])
(def dse [1 1])

(def all-dirs #{dn ds dw de dnw dne dsw dse})

(def sym-dirs
  {\F #{ds de} \7 #{ds dw} \J #{dn dw} \L #{dn de} \- #{dw de} \| #{dn ds}})

(def cw-rh
  ;; symbol -> {source direction -> adjacent nodes}
  {\F {ds #{} de #{dw dn}}
   \7 {dw #{} ds #{dn de}}
   \J {dn #{} dw #{de ds}}
   \L {de #{} dn #{ds dw}}
   \- {de #{dn} dw #{ds}}
   \| {dn #{dw} ds #{de}}})

(defn add-adj [adj new-rc]
  (if (not (loop-set (idx new-rc)))
    (conj adj new-rc)
    adj))

(defn rc-move [[r c] [dr dc]]
  (let [r' (+ r dr) c' (+ c dc)]
    (when (and (< -1 r' rows) (< -1 c' cols)) [r' c'])))

(defn step-rh [{:keys [prev-idx adj] :as state} cur-idx]
  (let [cur-sym (nodes-raw cur-idx)
        cur-dirs (vec (sym-dirs cur-sym))
        cur-rc (rc cur-idx)
        prev-rc (->> [cur-dirs (cycle [cur-rc])]
                     (apply map #(vector %1 (idx (mapv + %1 %2))))
                     (filter #(= (peek %) prev-idx))
                     first
                     first)
        cur-adj (filter identity
                        (map #(rc-move cur-rc %)
                             ((cw-rh cur-sym) prev-rc)))]
    {:adj (reduce add-adj adj cur-adj)
     :prev-idx cur-idx}))

(def adj-to-loop
  (:adj (reduce step-rh
                {:prev-idx 11144 :adj #{}}
                (drop 1 loop-nodes))))

(defn expand-adj [adj rc]
  (->> (map rc-move (cycle [rc]) [dn ds de dw])
       (filter identity)
       (filter #(not (loop-set (idx %))))
       (into adj)))

(def in-the-loop
  (loop [adj adj-to-loop iter 0]
    (if (or (> iter 100) (> (count adj) 19600))
      adj
      (let [new-adj (reduce expand-adj adj adj)
            _ (println iter (count new-adj))]
        (if (= new-adj adj) adj (recur new-adj (inc iter)))))))

(mapv println (sort in-the-loop))

(count in-the-loop)
