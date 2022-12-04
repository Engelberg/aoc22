(ns aoc22.day4.core
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

(def input (for [line (str/split-lines (slurp "src/aoc22/day4/input.txt"))
                 :let [[assign1 assign2] (str/split line #","),
                       [lower1 upper1] (str/split assign1 #"-"),
                       [lower2 upper2] (str/split assign2 #"-")]]
             (mapv parse-long [lower1 upper1 lower2 upper2])))
             
(defnc fully-contained? [[lower1 upper1 lower2 upper2]]
  (or (<= lower1 lower2 upper2 upper1)
      (<= lower2 lower1 upper1 upper2)))

(def part1-solution (count (filter fully-contained? input)))

(defnc overlap? [[lower1 upper1 lower2 upper2]]
  (<= (max lower1 lower2) (min upper1 upper2)))

(def part2-solution (count (filter overlap? input)))


