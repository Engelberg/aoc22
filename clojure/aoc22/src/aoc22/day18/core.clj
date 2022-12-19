(ns aoc22.day18.core
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

(def input (set (for [l (str/split-lines (slurp "src/aoc22/day18/input.txt"))]
                  (read-string (str "[" l "]")))))

(def adjacencies [[0 0 1] [0 0 -1] [0 1 0] [0 -1 0] [1 0 0] [-1 0 0]])

(defnc neighbors [pt]
  (map #(mapv + pt %) adjacencies))

(defnc count-surfaces [lava cube]
  (count (for [neighbor (neighbors cube)
               :when (not (lava neighbor))]
           1)))

(defnc surface-area [lava]
  (apply + (for [cube lava] (count-surfaces lava cube))))

(println (surface-area input))

(defnc air-cubes-touching-lava "cubes of air adjacent to lava" [lava]
  (set (for [cube lava, neighbor (neighbors cube)
             :when (not (lava neighbor))]
         neighbor)))

(defnc bounding-box [lava]
  :let [xs (mapv #(nth % 0) lava), ys (mapv #(nth % 1) lava), zs (mapv #(nth % 2) lava),
        min-x (dec (apply min xs)) max-x (inc (apply max xs))
        min-y (dec (apply min ys)) max-y (inc (apply max ys))
        min-z (dec (apply min zs)) max-z (inc (apply max zs))]
  (set (for [x (irange min-x max-x), y (irange min-y max-y), z (irange min-z max-z)] [x y z])))

(defnc connected-air-cubes [lava]
  :let [air-cubes (set/difference (bounding-box lava) lava)
        connections (for [cube air-cubes, neighbor (neighbors cube) :when (air-cubes neighbor)] [cube neighbor])]
  (reduce (fn [uf [c1 c2]] (uf/connect uf c1 c2)) (apply uf/union-find air-cubes) connections))

(defnc interior-air-cubes [lava]
  :let [ac (air-cubes-touching-lava lava),        
        cac (connected-air-cubes lava),
        outside (mapv + [-1 0 0] (first (sort lava)))]
  (for [cube ac, :when (not (uf/connected? cac cube outside))]
    cube))

(defnc precise-surface-area [lava]
  :let [iac (interior-air-cubes lava)
        lava-with-interior (set/union lava iac)]
  (apply + (for [cube input] (count-surfaces lava-with-interior cube))))

(println (precise-surface-area input))
