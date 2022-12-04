(ns aoc22.day3.core
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

(def input (str/split-lines (slurp "src/aoc22/day3/input.txt")))

(defnc priority [c]
  :let [n (int c)]
  (>= n 97) (- n 96)
  :else (- n 38))

(def solution-part1 (apply + (for [line input
                                   :let [[comp1 comp2] (split-at (quot (count line) 2) line)]]
                               (priority (first (set/intersection (set comp1) (set comp2)))))))

(def solution-part1 (transduce (map #(priority (first (apply set/intersection (map set (split-at (quot (count %) 2) %)))))) + input))

(def solution-part2 (apply + (for [group (partition 3 input)]
                               (priority (first (apply set/intersection (map #(set (nth group %)) (range 3))))))))
