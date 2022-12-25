(ns aoc22.day25.core
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

(def input (str/split-lines (slurp "src/aoc22/day25/input.txt")))

(def snafu-digits {\0 0, \1 1, \2 2, \- -1, \= -2})
(def rev-snafu-digits (set/map-invert snafu-digits))

(defnc snafu->base10 [s]
  :let [r (seq (reverse s))]
  (loop [power 1, r r, total 0]
    (cond
      (nil? r) total
      (recur (* power 5) (next r)
             (+ total (* power (snafu-digits (first r))))))))

(defnc snafu-mod [n]
  :let [n (mod n 5)]
  (= n 3) -2, (= n 4) -1
  :else n)

(defnc base10->snafu [n]
  (loop [n n, s ()]
    (cond
      (< n 3) (apply str (conj s (rev-snafu-digits n)))
      :let [d (snafu-mod n)]
      (recur (quot (- n d) 5) (conj s (rev-snafu-digits d))))))

(defnc solution-part-1 []
  (base10->snafu (apply + (map snafu->base10 input))))

