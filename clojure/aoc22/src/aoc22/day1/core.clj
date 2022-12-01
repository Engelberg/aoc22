(ns aoc22.day1.core
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

(def input (as-> (slurp "src/aoc22/day1/input.txt") %
             (str/split % #"\n\n")
             (map str/split-lines %)
             (map #(map read-string %) %)))

(def total-calories-per-elf (map #(reduce + %) input))

(def part1-solution (apply max total-calories-per-elf))

(def part2-solution (->> total-calories-per-elf
                         (sort >)
                         (take 3)
                         (apply +)))


