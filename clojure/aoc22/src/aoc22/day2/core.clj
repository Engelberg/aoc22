(ns aoc22.day2.core
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

(def input (for [line (str/split-lines (slurp "src/aoc22/day2/input.txt"))]
             (str/split line #" ")))

(def opponent-rps {"A" :rock, "B" :paper, "C" :scissors})
(def player-rps {"X" :rock, "Y" :paper, "Z" :scissors})
(def scoring-table {:rock 1, :paper 2, :scissors 3,
                    :win 6, :draw 3, :lose 0})

(defnc interpret-entry [[o p]] [(opponent-rps o) (player-rps p)])

(def beats? #{[:paper :rock] [:rock :scissors] [:scissors :paper]})

(defnc result [[opponent-throw player-throw]]
  (beats? [opponent-throw player-throw]) :lose
  (beats? [player-throw opponent-throw]) :win
  :else :draw)

(defnc rps-score [throws]
  (+ (scoring-table (peek throws)) (scoring-table (result throws))))

(def solution-part1 (transduce (comp (map interpret-entry) (map rps-score)) + input))

(def player-strategy {"X" :lose, "Y" :draw, "Z" :win})

(defnc interpret-entry-part2 [[o p]]
  :let [opponent-throw (opponent-rps o),
        player-result (player-strategy p)]
  [opponent-throw
   (first (for [throw [:rock :paper :scissors]
                :when (= (result [opponent-throw throw]) player-result)]
            throw))])

(def solution-part2 (transduce (comp (map interpret-entry-part2) (map rps-score)) + input))


