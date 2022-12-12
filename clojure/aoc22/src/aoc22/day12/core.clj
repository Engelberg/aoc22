(ns aoc22.day12.core
  (:refer-clojure :exclude [cond if-let if-some when-let when-some])
  (:require [better-cond.core :refer [cond if-let if-some when-let when-some defnc defnc-]]
            [ubergraph.alg :as alg]))

(def input (vec (str/split-lines (slurp "src/aoc22/day12/input.txt"))))

(def ROWS (count input))
(def COLS (count (first input)))

(defnc get-char [[x y]] (nth (nth input x) y))
(defnc in-grid? [[x y]] (and (< -1 x ROWS) (< -1 y COLS)))
(defnc elevation [pt] (let [c (get-char pt)] ({\S (int \a), \E (int \z)} c (int c))))

(defnc neighbors [pt]
  (for [v [[0 1] [1 0] [0 -1] [-1 0]]
        :let [neighbor (mapv + pt v)]
        :when (and (in-grid? neighbor) (<= (elevation neighbor) (inc (elevation pt))))]
    {:dest neighbor}))

(defnc find-pts [c]
  (for [x (range ROWS), y (range COLS), :when (= c (get-char [x y]))] [x y]))

(def start (first (find-pts \S)))
(def end (first (find-pts \E)))

(defnc solution-part-1 [] (:cost (alg/shortest-path neighbors start end)))

(def all-starts (cons start (find-pts \a)))

(defnc solution-part-2 [] (:cost (alg/shortest-path neighbors {:start-nodes all-starts, :end-node end})))

