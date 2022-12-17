(ns aoc22.day17.core
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

(def input (str/trim (slurp "src/aoc22/day17/input.txt")))
(def gases (cycle (med/indexed input)))
(def rock1 [[0 0] [1 0] [2 0] [3 0]])
(def rock2 [[1 0] [0 1] [1 1] [2 1] [1 2]])
(def rock3 [[0 0] [1 0] [2 0] [2 1] [2 2]])
(def rock4 [[0 0] [0 1] [0 2] [0 3]])
(def rock5 [[0 0] [0 1] [1 0] [1 1]])
(def rocks (cycle (med/indexed [rock1 rock2 rock3 rock4 rock5])))
(def starting-cave (into #{} (for [i (range 7)] [i 0])))
(defnc max-height [cave] (apply max (map peek cave)))
(defnc v+ [[a b] [c d]] [(+ a c) (+ b d)])

(defnc rock-appears [cave rock]
  :let [h (max-height cave)]
  (mapv (partial v+ [2 (+ 4 h)]) rock))

(defnc invalid-rock? [cave rock]
  (or (not (every? #(<= 0 % 6) (map first rock)))
      (seq (set/intersection cave (set rock)))))

(defnc simulate-rock-fall [[cave rocks gases]]
  (loop [rock (rock-appears cave (peek (first rocks))), gases gases]
    (cond
      :let [gas (peek (first gases)),
            shifted-rock (if (= gas \<) (mapv (partial v+ [-1 0]) rock) (mapv (partial v+ [1 0]) rock)),
            shifted-rock (if (invalid-rock? cave shifted-rock) rock shifted-rock)
            next-rock (mapv (partial v+ [0 -1]) shifted-rock)]
      (invalid-rock? cave next-rock) [(into cave shifted-rock) (next rocks) (next gases)]
      :else (recur next-rock (next gases)))))

(def simulation (map first (iterate simulate-rock-fall [starting-cave rocks gases])))

(defn print-cave "For debugging" [cave]
  (let [h (max-height cave)]
    (doseq [y (range h 0 -1)]
      (doseq [x (range 0 7)]
        (if (cave [x y]) (print \#) (print \.)))
      (newline))))

(defn solution-part-1 [] (max-height (nth simulation 2022)))

(defnc top-rows [cave]
  :let [h (max-height cave)]
  (into #{} (for [i (range -50 1),
                  :let [y (+ h i)],
                  x (range 0 6)
                  :when (cave [x y])]
              [x i])))

(defnc find-repeat []
  (loop [simulation (iterate simulate-rock-fall [starting-cave rocks gases]), i 0, seen {}]
    (cond
      :let [[cave rocks gases] (first simulation), simulation (next simulation),
            state [(top-rows cave) (first (first rocks)) (first (first gases))],
            seen-before (seen state)]
      seen-before [seen-before i state]
      :else (recur simulation (inc i) (assoc seen state i)))))

(def target 1000000000000)

(defnc compute-height [target [lb hb]]
  :let [period (- hb lb),
        num-periods (quot (- target lb) period),
        period-height (- (max-height (nth simulation hb)) (max-height (nth simulation lb))),
        residual (mod (- target lb) period)]
  :do (assert (= (+ lb (* period num-periods) residual) target))
  (+ (bigint (max-height (nth simulation (+ residual lb))))
     (* period-height num-periods)))

(defn solution-part-2 []
  (compute-height target (find-repeat)))

