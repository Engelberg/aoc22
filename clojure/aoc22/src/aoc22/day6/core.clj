(ns aoc22.day6.core
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

(def input (slurp "src/aoc22/day6/input.txt"))

(defnc find-start [s n]
  (+ n (ffirst (filter (fn [[i p]] (= (count (set p)) n)) (med/indexed (partition n 1 s))))))

(println (find-start input 4))
(println (find-start input 14))
