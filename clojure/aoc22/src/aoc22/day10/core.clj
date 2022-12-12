(ns aoc22.day10.core
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

(def input (for [l (str/split-lines (slurp "src/aoc22/day10/input.txt"))]
             (cond (= l "noop") :noop
                   :let [[ins n] (str/split l #"\s")]
                   [(keyword ins) (parse-long n)])))

(defnc step [register-history instruction]
  :let [current-x (peek register-history)]
  (= instruction :noop) (conj register-history current-x)
  :let [[_ n] instruction]
  (into register-history [current-x (+ current-x n)]))

(defnc simulate [instructions]
  (reduce step [1 1] instructions))

(def simulation (simulate input))

(defnc signal-strength [i] (* i (simulation i)))

(def solution-part-1 (transduce (map signal-strength) + (range 20 221 40)))

(defnc neighbor? [x y] (<= (abs (- x y)) 1))

(def screen (vec (for [i (range 240)
                       :let [pixel (simulation (inc i))]]
                   (if (neighbor? (mod i 40) pixel) \# \.))))

(newline) (newline)
(doseq [i (range 0 202 40)]
  (println (apply str (subvec screen i (+ 40 i)))))
