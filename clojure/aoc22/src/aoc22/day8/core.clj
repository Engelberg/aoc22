(ns aoc22.day8.core
  (:refer-clojure :exclude [cond if-let if-some when-let when-some])
  (:require [better-cond.core :refer [cond if-let if-some when-let when-some defnc defnc-]]
            [clojure.math.numeric-tower :as nt]
            [clojure.math :as math]
            [medley.core :as med]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [instaparse.core :as insta]
            [clojure.data.priority-map :as pm]
            [com.rpl.specter :refer :all]))

(def input (str/split-lines (slurp "src/aoc22/day8/input.txt")))

(def NUM-ROWS (count input))
(def NUM-COLS (count (first input)))

(defnc in-grid? [[x y]] (and (< -1 x NUM-ROWS) (< -1 y NUM-COLS)))
(defnc v+ [[a b] [c d]] [(+ a c) (+ b d)])

(def grid (into [] (for [i (range NUM-ROWS)] (vec (for [j (range NUM-COLS)] (parse-long (str (nth (nth input i) j))))))))

(defnc visible [[x y] v]
  (loop [[x y] [x y], height ((grid x) y) visible-trees #{[x y]}]
    (cond
      :let [[x y] (v+ [x y] v)]
      (not (in-grid? [x y])) visible-trees
      :let [new-height ((grid x) y)]
      (> new-height height) (recur [x y] new-height (conj visible-trees [x y]))
      :else (recur [x y] height visible-trees))))

(defnc all-visible-trees []
  (apply set/union
         (concat
          (for [y (range NUM-COLS)] (visible [0 y] [1 0]))
          (for [y (range NUM-COLS)] (visible [(dec NUM-ROWS) y] [-1 0]))
          (for [x (range NUM-ROWS)] (visible [x 0] [0 1]))
          (for [x (range NUM-ROWS)] (visible [x (dec NUM-COLS)] [0 -1])))))

(def solution-part-1 (count (all-visible-trees)))

(defnc scenic-trees [[x y] v]
  :let [height ((grid x) y)]
  (loop [[x y] [x y] trees #{}]
    (cond
      :let [[x y] (v+ [x y] v)]
      (not (in-grid? [x y])) trees
      (< ((grid x) y) height) (recur [x y] (conj trees [x y]))
      :else (conj trees [x y]))))

(defnc scenic-score [[x y]]
  (apply * (map #(count (scenic-trees [x y] %)) [[1 0] [0 1] [-1 0] [0 -1]])))

(def solution-part-2 (apply max (for [x (range NUM-ROWS) y (range NUM-COLS)] (scenic-score [x y]))))
