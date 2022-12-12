(ns aoc22.day11.core
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

(def input (vec (for [block (str/split (slurp "src/aoc22/day11/input.txt") #"\n\n")
                      :let [[l1 l2 l3 l4 l5 l6] (str/split-lines block),
                            monkey (read-string (last (str/split l1 #"\s+|:")),)
                            starting-items (med/queue (map read-string (drop 3 (str/split l2 #"\s+|,\s+"))))
                            [arg1 op arg2] (map read-string (drop 4 (str/split l3 #"\s+"),))
                            divisor (read-string (last (str/split l4 #"\s+"))),
                            true-monkey (read-string (last (str/split l5 #"\s+"))),
                            false-monkey (read-string (last (str/split l6 #"\s+")))]]
                  {:items starting-items, :worry-fn (eval (list 'fn '[old] (list op arg1 arg2))),
                   :divisor divisor, :true-monkey true-monkey, :false-monkey false-monkey, :inspections 0})))

(def ^:dynamic *part1* true)
(def common-divisor (transduce (map :divisor) * input))

(defnc simulate-monkey [monkeys monkey]
  :let [{:keys [items worry-fn divisor true-monkey false-monkey]} (nth monkeys monkey)]
  (empty? items) monkeys,
  :let [item (peek items),
        worry (if *part1* (quot (worry-fn item) 3) (mod (worry-fn item) common-divisor)),        
        test? (zero? (mod worry divisor)),
        next-monkey (if test? true-monkey false-monkey),
        monkeys (->> monkeys (multi-transform (multi-path
                                               [monkey (multi-path [:items (terminal pop)]
                                                                   [:inspections (terminal inc)])]
                                               [next-monkey :items (terminal #(conj % worry))])))]
  (recur monkeys monkey))

(defnc simulate-round [monkeys] (reduce simulate-monkey monkeys (range (count monkeys))))

(defnc solution-part-1 []
  :let [[monkey1 monkey2] (sort-by :inspections > (nth (iterate simulate-round input) 20))]
  (* (:inspections monkey1) (:inspections monkey2)))

(defnc solution-part-2 []
  (binding [*part1* false]
    (let [[monkey1 monkey2] (sort-by :inspections > (nth (iterate simulate-round input) 10000))]
      (* (:inspections monkey1) (:inspections monkey2)))))
