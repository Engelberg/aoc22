(ns aoc22.day19.core
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
            [com.rpl.specter :refer :all]
            [engelberg.data.union-find :as uf]))

(defnc irange [x y] (range x (inc y)))
(defnc v- [x y] (mapv - x y))
(defnc v+ [x y] (mapv + x y))
(defnc andf [x y] (and x y))

(def input (vec (for [l (str/split-lines (slurp "src/aoc22/day19/input.txt"))
                      :let [[a b c d e f g] (map parse-long (re-seq #"[0-9]+" l))]]
                  [[b 0 0 0] [c 0 0 0] [d e 0 0] [f 0 g 0]])))

(def NUM-BLUEPRINTS (count input))

(def initial-state [0 [1 0 0 0] [0 0 0 0]])

(defnc geodes [state] (peek (peek state)))

(defnc buildable? [robots cost]
  (reduce andf
          (for [i (range 4)
                :when (pos? (cost i))]
            (pos? (robots i)))))

(defnc build [blueprint [i robots minerals plan] robot]
  :let [cost (nth blueprint robot)]
  (not (buildable? robots cost)) nil
  (loop [i i, minerals minerals]
    (cond      
      (reduce andf (map <= cost minerals))
      [(inc i) (transform robot inc robots) (v+ robots (v- minerals cost))],
      :else (recur (inc i) (v+ minerals robots)))))

(defnc max-minerals-needed [blueprint]
  (vec (for [i (range 4)] (apply max (map #(nth % i) blueprint)))))
(def max-minerals-needed (memoize max-minerals-needed))

(defnc useless-build? [blueprint [time robots minerals :as state] i cutoff]
  (= i 3) false
  :let [max-minerals (max-minerals-needed blueprint)
        time-remaining (- cutoff time)]
  (or 
   (>= (robots i) (max-minerals i))
   (>= (+ (minerals i) (* time-remaining (robots i))) (* (max-minerals i) time-remaining))))

(defnc next-states [blueprint [time robots minerals :as state] cutoff]
  (= time cutoff) nil
  :let [states (for [robot (range 0 4)
                     :when (not (useless-build? blueprint state robot cutoff))
                     :let [s (build blueprint state robot)]
                     :when (and s (<= (nth s 0) cutoff))]
                 s)]
  (seq states) states
  :else [[cutoff robots (reduce v+ minerals (repeat (- cutoff time) robots))]])

(defnc max-geodes [blueprint [time robots minerals :as state] cutoff]
  :let [time-remaining (- cutoff time),
        max-minerals (max-minerals-needed blueprint)]
  (loop [time time robots robots minerals-copied (vec (repeat 4 (vec minerals)))]
    (cond
      (>= time cutoff) (peek (minerals-copied 0))
      :let [new-robots (vec (for [i (range 4)]
                              (cond
                                :let [minerals (minerals-copied i)]
                                (useless-build? blueprint state i cutoff) 0
                                (if (reduce andf (mapv <= (blueprint i) minerals)) 1 0)))),
            new-minerals (vec (for [i (range 4)]
                                (cond
                                  :let [minerals (minerals-copied i)]
                                  (= (new-robots i) 1) (v- (v+ robots minerals) (blueprint i))
                                  :else (v+ robots minerals))))]
      (recur (inc time) (v+ robots new-robots) new-minerals))))

(defnc search-states [blueprint state cutoff]
  (loop [states [state], best 0]
    (cond
      (= (count states) 0) best
      :let [s (peek states), ns (next-states blueprint s cutoff),
            states (into (pop states) (filter #(> (max-geodes blueprint % cutoff) best)) ns), g (geodes s)]
      (> g best) (do (println g) (recur states g))
      :else (recur states best))))

(defnc solution-part-1 []
  (apply + 
         (for [i (range NUM-BLUEPRINTS)
               :let [blueprint (input i),
                     _ (println i)
                     num-geodes (search-states blueprint initial-state 24)]]
           (* (inc i) num-geodes))))

(defnc solution-part-2 []
  (apply * (mapv #(search-states (nth input %) initial-state 32) (range 3))))
