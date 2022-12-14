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

(def input (for [l (str/split-lines (slurp "src/aoc22/day14/input.txt")),
                 :let [coords (map (fn [c] (mapv parse-long (str/split c #",")))
                                   (str/split l #" -> "))]
                 rock-line (partition 2 1 coords)]
             rock-line))

(defnc build-rockline [[[x1 y1] [x2 y2]]]
  (= x1 x2) (into {} (for [y (range (min y1 y2) (inc (max y1 y2)))]
                       [[x1 y] :rock]))
  (= y1 y2) (into {} (for [x (range (min x1 x2) (inc (max x1 x2)))]
                       [[x y1] :rock])))

(def rockmap (apply merge (map build-rockline input)))
(def MAXDEPTH (apply max (map peek (keys rockmap))))
(def ^:dynamic *part1* true)

(defnc simulate-sand-step [rockmap [x y :as sand]]
  (rockmap sand) rockmap
  (> y MAXDEPTH) (if *part1* rockmap (assoc rockmap sand :sand))
  (nil? (rockmap [x (inc y)])) (recur rockmap [x (inc y)])
  (nil? (rockmap [(dec x) (inc y)])) (recur rockmap [(dec x) (inc y)])
  (nil? (rockmap [(inc x) (inc y)])) (recur rockmap [(inc x) (inc y)])
  :else (assoc rockmap sand :sand))

(defnc final-rockmap [rockmap]
  (loop [rockmap rockmap]
    (cond
      :let [next-rockmap (simulate-sand-step rockmap [500 0])]
      (= rockmap next-rockmap) rockmap
      :else (recur next-rockmap))))

(defnc count-sand [rockmap] (count (filter #{:sand} (vals rockmap))))

(def solution-part-1 (count-sand (final-rockmap rockmap)))
(def solution-part-2 (binding [*part1* false] (count-sand (final-rockmap rockmap))))
