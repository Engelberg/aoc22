(ns aoc22.day24.core
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
(def blizzard-directions {\> east, \^ north, \< west, \v south})
(def rev-blizzard-directions (set/map-invert blizzard-directions))

(def input (str/split-lines (slurp "src/aoc22/day24/input.txt")))
(def ROWS (- (count input) 2))
(def COLS (- (count (first input)) 2))
(def PERIOD 600) ;; Found by doing (count (set (take (* ROWS COLS) (iterate update-blizzards blizzards))))

(defnc v+-wrap [[a b] [c d]]
  [(mod (+ a c) ROWS) (mod (+ b d) COLS)])

(def blizzards (into {} (for [[row line] (med/indexed input)
                              col (range (count line))
                              :let [character (nth line col)]                              
                              :when (contains? blizzard-directions character)]
                          [[(dec row) (dec col)] [(blizzard-directions character)]])))

(defnc in-grid? [[x y :as pt]]
  (or (and (< -1 x ROWS) (< -1 y COLS))
      (#{[-1 0] [ROWS (dec COLS)]} pt)))

(defnc neighbors [pt]
  (for [dir [east west north south [0 0]]
        :let [new-pt (v+ pt dir)]
        :when (in-grid? new-pt)]
    new-pt))

(defnc update-blizzards [bs]
  (apply merge-with into (for [[pt dirs] bs, dir dirs] {(v+-wrap pt dir) [dir]})))

(def all-possible-blizzards (vec (take PERIOD (iterate update-blizzards blizzards))))

(def start-cell [-1 0])
(def end-cell [ROWS (dec COLS)])
(def initial-state [start-cell 0])

(defnc next-state [[me blizzard-i]]
  :let [blizzard-i (mod (inc blizzard-i) PERIOD),
        me-candidates (remove (nth all-possible-blizzards blizzard-i) (neighbors me))]
  (for [pt me-candidates] {:dest [pt blizzard-i]}))

(defnc end-state? [target]
  (fn [[me _]] (= me target)))

(defnc manhattan-distance [[a b] [c d]]
  (+ (abs (- a c)) (abs (- b d))))

(defnc lower-bound [target]
  (fn [[me _]] (manhattan-distance me target)))

(def one-way (alg/shortest-path next-state {:start-node initial-state, :end-node? (end-state? end-cell)
                                            :heuristic-fn (lower-bound end-cell)}))

(defnc solution-part-1 [] (:cost one-way))

(defnc solution-part-2 []
  :let [{trip1 :end, cost1 :cost} one-way
        {trip2 :end, cost2 :cost} (alg/shortest-path next-state
                                                     {:start-node trip1,
                                                      :end-node? (end-state? start-cell)
                                                      :heuristic-fn (lower-bound start-cell)})
        {trip3 :end, cost3 :cost} (alg/shortest-path next-state
                                                     {:start-node trip2, :end-node? (end-state? end-cell)
                                                      :heuristic-fn (lower-bound end-cell)})]
  (+ cost1 cost2 cost3))


;; For debugging

(defn print-state [[me blizzard-i]]
  (let [blizzards (nth all-possible-blizzards blizzard-i)]
    (doseq [row (range ROWS)]
      (doseq [col (range COLS)]
        (cond
          (= me [row col]) (print \E)
          :let [dirs (blizzards [row col])]
          (nil? dirs) (print \.)
          (= (count dirs) 1) (print (rev-blizzard-directions (first dirs)))
          :else (print (count dirs))))
      (newline))
    (newline)))
