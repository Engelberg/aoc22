(ns aoc22.day13.core
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

(def input (for [[l1 l2 _] (partition-all 3 (str/split-lines (slurp "src/aoc22/day13/input.txt")))]
             [(read-string l1) (read-string l2)]))

(defnc packet-compare [l1 l2]
  (and (integer? l1) (integer? l2)) (compare l1 l2)
  (and (seqable? l1) (seqable? l2)) (cond (and (empty? l1) (empty? l2)) 0
                                          (empty? l1) -1
                                          (empty? l2) 1
                                          :let [f1 (first l1), f2 (first l2),
                                                compare-items (packet-compare f1 f2)]
                                          (not= 0 compare-items) compare-items
                                          :else (recur (rest l1) (rest l2)))
  (integer? l1) (recur [l1] l2)
  (integer? l2) (recur l1 [l2]))

(def solution-part-1 (transduce (map-indexed (fn [i [l1 l2]] (if (= -1 (packet-compare l1 l2)) (inc i) 0))) + input))

(def solution-part-2 (apply * (for [[i v] (med/indexed (sort packet-compare
                                                             (into (apply concat input) [[[2]] [[6]]])))
                                    :when (contains? #{[[2]] [[6]]} v)]
                                (inc i))))
