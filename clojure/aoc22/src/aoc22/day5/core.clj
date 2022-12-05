(ns aoc22.day5.core
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

(def stacks [() "NTBSQHGR" "JZPDFSH" "VHZ" "HGFJZM" "RSMLDCZT" "JZHVWTM" "ZLPFT" "SWVQ" "CNDTMLHW"])

(def input (for [move (drop 10 (str/split-lines (slurp "src/aoc22/day5/input.txt")))]
             (filterv identity (map parse-long (str/split move #" ")))))

(defnc process-move [stacks [quantity from to]]
  :let [crates (take quantity (nth stacks from))]
  (->> stacks
       (transform from #(drop quantity %))
       (transform to #(concat (reverse crates) %))))

(def solution-part1 (apply str (rest (map first (reduce process-move stacks input)))))

(defnc process-move2 [stacks [quantity from to]]
  :let [crates (take quantity (nth stacks from))]
  (->> stacks
       (transform from #(drop quantity %))
       (transform to #(concat crates %))))

(def solution-part2 (apply str (rest (map first (reduce process-move2 stacks input)))))
