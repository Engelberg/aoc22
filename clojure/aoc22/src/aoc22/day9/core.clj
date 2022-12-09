(ns aoc22.day9.core
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

(def input (for [l (str/split-lines (slurp "src/aoc22/day9/input.txt"))
                 :let [[dir n] (str/split l #"\s")]]
             [(keyword dir) (parse-long n)]))

(def dv {:R [1 0], :L [-1 0], :U [0 -1], :D [0 1]})
(defnc neighbor? [[x1 y1] [x2 y2]] (and (<= (abs (- x1 x2)) 1) (<= (abs (- y1 y2)) 1)))

(defnc move-tail [head tail]
  (neighbor? head tail) tail
  (mapv + tail (map compare head tail)))

(defnc simulate-step [rope dir]
  :let [head (mapv + (rope 0) (dv dir))]
  (loop [new-rope [head], rope (next rope)]
    (cond
      (nil? rope) new-rope 
      :let [next-knot (move-tail (peek new-rope) (first rope))]
      (recur (conj new-rope next-knot) (next rope)))))

(defnc unroll-directions [directions] (mapcat (fn [[dir n]] (repeat n dir)) directions))

(defnc count-tail-positions [num-links]
  (count (into #{} (map peek) (reductions simulate-step (vec (repeat num-links [0 0]))
                                          (unroll-directions input)))))

(println (count-tail-positions 2))
(println (count-tail-positions 10))
