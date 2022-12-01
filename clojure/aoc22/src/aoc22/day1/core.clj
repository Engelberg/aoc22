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
            [clojure.data.priority-map :as pm]
            [com.rpl.specter :refer :all]))

(def input (for [block (str/split (slurp "src/aoc22/day1/input.txt") #"\n\n")]
             (map read-string (str/split-lines block))))

;; ((8462 6981 3714 4409 8186 3614 2218 7558 6702)
;;  (2947 4727 6396 5718 2361 1970 1583 2816 2995 6914 4313 1401)
;;  (1643 7815 2162 8841 7671)
;;  ...)

(def total-calories-per-elf (map #(apply + %) input))

(def part1-solution (apply max total-calories-per-elf))

(def part2-solution (->> total-calories-per-elf (sort >) (take 3) (apply +)))


