(ns aoc22.day15.core
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

(def input-parser (insta/parser "<S> = <'Sensor at x='> NUM <', y='> NUM <': closest beacon is at x='> NUM <', y='> NUM
                           <NUM> = #'-?[0-9]+'"))

(def input (for [[sx sy bx by] (map input-parser (str/split-lines (slurp "src/aoc22/day15/input.txt")))]
             [(mapv parse-long [sx sy]) (mapv parse-long [bx by])]))

(defnc manhattan-distance [[x1 y1] [x2 y2]] (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defnc sensor-range "Returns the (inclusive) range of xs on row y where no defective beacon can be"
  [y [[sx sy :as sensor] beacon]]
  :let [d (manhattan-distance sensor beacon),
        distance-to-yrow (abs (- y sy))]
  (> distance-to-yrow d) nil
  [(- sx (- d distance-to-yrow)) (+ sx (- d distance-to-yrow))])

(defnc merge-ranges "Assumes ranges are in sorted order" [ranges [x3 x4]]
  (empty? ranges) [[x3 x4]]
  :let [[x1 x2] (peek ranges)]
  (= x2 x3) (conj (pop ranges) [x1 x4])
  (< x2 x3) (conj ranges [x3 x4])
  (> x2 x3) (if (<= x4 x2) ranges (conj (pop ranges) [x1 x4])))

(defnc no-beacon-ranges [measurements y]
  (reduce merge-ranges [] (sort (keep (partial sensor-range y) measurements))))

(defn size-range [[x1 x2]] (inc (- x2 x1)))

(def beacons-by-row (apply merge-with into (for [[bx by] (set (map second input))] {by [[bx by]]})))

(defnc solution-part-1 []
  (- (apply + (map size-range (no-beacon-ranges input 2000000)))
     (count (beacons-by-row 2000000))))

(defnc beacon-ruled-out? [ranges]
  (some (fn [[x1 x2]] (<= x1 0 4000000 x2)) ranges))

(defnc find-beacon [ranges]
  (first (for [[[x1 x2] [x3 x4]] (partition 2 ranges)
               :when (<= 0 x2 x3 4000000)]
           (inc x2))))

(defnc tuning-frequency [x y]
  (+ (* x 4000000) y))

(defnc solution-part-2 []
  (first (for [y (range 4000000 -1 -1)
               :let [ranges (no-beacon-ranges input y)]
               :when (not (beacon-ruled-out? ranges))]
           (tuning-frequency (find-beacon ranges) y))))

;; Alternative parallel version, running on 4 threads, just for fun

(defnc tuning-frequency-of-defective-beacon [[low high] f]
  (.start (Thread.
           (fn [] (loop [y low]
                    (cond
                      (or (realized? f) (= y high)) nil
                      :let [ranges (no-beacon-ranges input y)]
                      (beacon-ruled-out? ranges) (recur (inc y))
                      :else (deliver f (tuning-frequency (find-beacon ranges) y))))))))

(defnc solution-part-2 []
  :let [f (promise)]
  :do (doseq [interval (partition 2 1 (conj (vec (range 0 4000000 1000000)) 4000001))]
        (tuning-frequency-of-defective-beacon interval f))
  @f)
