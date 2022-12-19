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

(def MAXTIME 32)

(def input (vec (for [l (str/split-lines (slurp "src/aoc22/day19/sample.txt"))
                      :let [[a b c d e f g] (map parse-long (re-seq #"[0-9]+" l))]]
                  [[b 0 0 0] [c 0 0 0] [d e 0 0] [f 0 g 0]])))

(def NUM-BLUEPRINTS (count input))

(def ore-nth #(nth % 0))
(def clay-nth #(nth % 1))
(def obsidian-nth #(nth % 2))
(def geode-nth #(nth % 3))

(defnc next-states [blueprint [robots minerals]]
  :let [minerals (v+ minerals robots)]
  (cons [robots minerals]
        (for [robot (range 3 -1 -1),
              :let [cost (nth blueprint robot)]
              :when (reduce andf (map <= cost minerals))]
          [(transform robot inc robots) (v- minerals cost)])))

(def initial-state [0 [1 0 0 0] [0 0 0 0]])
(def blueprint1 (first input))

(defnc geodes [state] (peek (peek state)))

(defn generation [blueprint states]
  (sort-by geodes > (distinct (mapcat #(next-states blueprint %) states))))

(defnc after-n-minutes [blueprint n]
  (nth (iterate #(generation blueprint %) [initial-state]) n))

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
      (let [ns [(inc i) (transform robot inc robots) (v+ robots (v- minerals cost))]]
        ns #_(conj ns (conj plan ns)))
      :else (recur (inc i) (v+ minerals robots)))))

(defnc next-states [blueprint [time robots minerals :as state] cutoff]
  (= time cutoff) nil
  :let [states (for [robot (range 0 4)
                     :let [s (build blueprint state robot)]
                     :when (and s (<= (nth s 0) cutoff))]
                 s)]
  (seq states) states
  :else [[cutoff robots (reduce v+ minerals (repeat (- cutoff time) robots))]])

(defnc geode-bots [s] (peek (nth s 1)))
(defnc min-time-to-build-geode-robot [blueprint initial-states]
  :let [g (geode-bots (first initial-states))]
  (alg/shortest-path (fn [s] (for [state (next-states blueprint s MAXTIME)] {:dest state,
                                                                        :weight (- (nth state 0) (nth s 0))}))
                     {:start-nodes initial-states, :end-node? #(> (geode-bots %) g), :cost-attr :weight}))

(defnc sum-from [x y]
  (<= y x) 0
  (- (quot (+ y (inc y)) 2)
     (quot (+ x (inc x)) 2)))

;; (defnc max-geodes [blueprint [time robots minerals :as state] min-time]
;;   :let [time-remaining (- MAXTIME time)]
;;   (+ (geodes state) (* (peek robots) time-remaining)
;;      (apply + (for [i (range (+ time min-time) MAXTIME min-time)]
;;                 (- MAXTIME i)))))

(defnc max-geodes [blueprint [time robots minerals :as state]]
  :let [time-remaining (- MAXTIME time)]
  (+ (geodes state) (sum-from (+ time 2) MAXTIME) (* (peek robots) time-remaining)))


(defnc search-states [blueprint state]
  (loop [states [state], best 0]
    (cond
      (= (count states) 0) best
      :let [s (peek states), ns (next-states blueprint s), states (into (pop states) ns), g (geodes s)]
      (<= (max-geodes blueprint1 s) best) (recur states best)
      ;;:do (println s)
      (> g best) (do (println g) (recur states g))
      :else (recur states best))))

(defnc all-states [blueprint states cutoff num-geode-bots]
  ;;  :do (println "all-states" blueprint states cutoff num-geode-bots)
  (loop [states (vec states), cutoff-states []]
    (cond
      (= (count states) 0) (distinct (filter #(= (geode-bots %) num-geode-bots) cutoff-states))
      :let [[time robots minerals :as s] (peek states), states (pop states)]
      (= time MAXTIME) [s]
      (= time cutoff) (recur states (conj cutoff-states s))
      :let [ns (next-states blueprint s cutoff)]
      (seq ns) (recur (into states ns) cutoff-states)
      :else (recur (conj states [cutoff robots (reduce v+ minerals (repeat (- cutoff time) robots))]) cutoff-states))))

(defnc greedy [blueprint state]
  (loop [states [state]]    
    (cond
      ;; :do (println states)
      :let [[time robots minerals] (first states)]
      (= time MAXTIME) (geodes (first states))
      :let [build-another-geode-robot (:end (min-time-to-build-geode-robot blueprint states)),
            time-to-build-geode-robot (first build-another-geode-robot)]
      ;;      :do (println states time-to-build-geode-robot (geode-bots build-another-geode-robot))
      (nil? build-another-geode-robot) (peek (reduce v+ minerals (repeat (- MAXTIME time) robots)))
      :let [next-states (all-states blueprint states time-to-build-geode-robot
                                    (geode-bots build-another-geode-robot))]
      ;;      :do (println "After")
      ;;      :do (flush)
      (recur next-states))))

(defnc solution-part-1 []
  (apply + 
         (for [i (range NUM-BLUEPRINTS)
               :let [blueprint (input i),
                     _ (println i)
                     num-geodes (greedy blueprint initial-state)]]
           (* (inc i) num-geodes))))

(defnc max-geodes-producible [time-left [robots minerals]]
  (= time-left 0) 0
  (= time-left 1) (peek robots)
