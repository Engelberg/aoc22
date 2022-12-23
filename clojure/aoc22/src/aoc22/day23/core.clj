(ns aoc22.day23.core
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
            [clojure.java.io :as io]))

(defnc irange [x y] (range x (inc y)))
(def v+ (partial mapv +))
(def east [0 1]) (def west [0 -1]) (def north [-1 0]) (def south [1 0])
(def northeast (v+ north east)) (def northwest (v+ north west))
(def southeast (v+ south east)) (def southwest (v+ south west))

(def input (str/split-lines (slurp "src/aoc22/day23/input.txt")))
(def ROWS (count input))
(def COLS (count (first input)))

(def grid (set (for [[row line] (med/indexed input)
                     col (range COLS)
                     :let [character (nth line col)]
                     :when (not= character \.)]
                 [row col])))

(defnc find-first-from-i [v i]
  (v i) (v i)
  :else (recur v (mod (inc i) (count v))))

(defnc proposal [grid priority elf]
  :let [[n e w s ne se nw sw :as neighbors]
        (map (partial v+ elf) [north east west south northeast southeast northwest southwest])]
  (not (some grid neighbors)) nil
  :let [considerations [(when-not (some grid [n ne nw]) north)
                        (when-not (some grid [s se sw]) south)
                        (when-not (some grid [w nw sw]) west)
                        (when-not (some grid [e ne se]) east)]]
  (every? nil? considerations) nil
  (find-first-from-i considerations priority))

(defnc proposals [grid priority]
  (apply merge-with into (reduce (fn [props elf] (if-let [prop (proposal grid priority elf)]
                                                   (conj props {(v+ elf prop) [elf]})
                                                   props))
                                 [] grid)))

(defnc resolve-proposal [grid [dest elves]]
  (= (count elves) 1) (let [elf (first elves)]
                        (conj (disj grid elf) dest))
  :else grid)

(defnc resolve-proposals [grid props]
  (reduce resolve-proposal grid props))

(defnc next-grid [[grid priority]]
  [(resolve-proposals grid (proposals grid priority)) (mod (inc priority) 4)])

(defnc grid-bounds [grid]
  :let [min-row (apply min (map first grid))
        max-row (apply max (map first grid))
        min-col (apply min (map second grid))
        max-col (apply max (map second grid))]
  [min-row max-row min-col max-col])

(defnc print-grid "For debugging" [grid]
  :let [[min-row max-row min-col max-col] (grid-bounds grid)]
  :do (doseq [row (irange min-row max-row)]
        (doseq [col (irange min-col max-col)]
          (if (grid [row col]) (print \#) (print \.)))
        (newline))
  :do (newline))

(defnc solution-part-1 []
  :let [[g _] (nth (iterate next-grid [grid 0]) 10),
        [min-row max-row min-col max-col] (grid-bounds g)]
  (- (* (inc (- max-row min-row)) (inc (- max-col min-col))) (count g)))

(defnc solution-part-2 []
  :let [simulation (iterate next-grid [grid 0]),
        comparisons (med/indexed (partition 2 1 simulation))]
  (first (for [[i [[g1 _] [g2 _]]] comparisons
               :when (= g1 g2)]
           (inc i))))
