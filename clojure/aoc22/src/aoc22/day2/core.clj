(ns aoc22.day2.core
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
            [clojure.data.priority-map :as pm]))

(def input (as-> (slurp "src/aoc22/day2/input.txt") %
                (str/split % #"\n\n")
                (map str/split-lines %)
                (mapv #(mapv read-string %) %)))

(def part1-solution (apply max (map #(reduce + %) input)))

(def part2-solution (as-> (map #(reduce + %) input) %
                      (sort > %)
                      (take 3 %)
                      (apply + %)))


