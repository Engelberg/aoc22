(ns aoc22.day16.core
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

(def input-parser (insta/parser "S = <'Valve '> Valve <' has flow rate='> Num <'; '> <#'\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+'> Valves
                                 Valve = #'[A-Z][A-Z]'
                                 Valves = Valve (<', '> Valve)*
                                 Num = #'[0-9]+'"))

(def input (for [l (str/split-lines (slurp "src/aoc22/day16/input.txt"))]
             (insta/transform {:Num parse-long, :Valve keyword, :Valves vector, :S vector}
                              (input-parser l))))

(def ^:dynamic *MAXTIME* 26)

(def valve-flow-map (into {} (for [[valve flow _] input] [valve flow])))
(def zero-valves (into #{} (for [[valve flow _] input :when (zero? flow)] valve)))

(def cave-graph (apply uber/digraph (for [[cave1 _ caves] input, cave2 caves] [cave1 cave2])))
(def all-caves (mapv first input))
(def distances (into {} (for [cave all-caves,
                              {:keys [cost end]} (alg/shortest-path cave-graph {:start-node cave :traverse true})]
                          [[cave end] cost])))

(defnc flow-benefit [cave time]
  (>= time *MAXTIME*) 0
  (* (- *MAXTIME* time) (valve-flow-map cave)))

(defrecord State [cave valves time flow path])
(def initial-state (->State :AA zero-valves 1 0 []))

(defnc complete? [{:keys [time]}] (>= time *MAXTIME*))
(defnc next-states [{:keys [cave valves time flow path] :as state}]
  :let [adjacent-caves (map uber/dest (uber/out-edges cave-graph cave)),
        moves-to-adjacent-caves (vec (for [dest adjacent-caves]
                                       (->State dest valves (inc time) flow (conj path [:move dest]))))]
  (valves cave) moves-to-adjacent-caves
  :let [flow-from-flipping (* (- *MAXTIME* time) (valve-flow-map cave))]
  :else (conj moves-to-adjacent-caves (->State cave (conj valves cave) (inc time) (+ flow flow-from-flipping) (conj path [:flip cave]))))

(def best-state (atom initial-state))

(def best-flow-info (atom {}))

(defnc add-to-best-flow-info [m [cave valves time flow]]
  (reduce (fn [m t] (transform [(keypath [cave valves t]) (nil->val -1)] #(max flow %) m)) m (range time *MAXTIME*)))

(defnc seen? [{:keys [cave valves flow time]}]
  :let [best-flow (@best-flow-info [cave valves time] -1)]
  (<= flow best-flow) true
  :do (swap! best-flow-info add-to-best-flow-info [cave valves time flow])
  false)

(defnc upper-bound [{:keys [cave valves time flow]}]
  (transduce (comp (remove valves) (map (fn [dest] (let [dist (distances [cave dest])] (flow-benefit dest (+ time dist)))))) + flow all-caves))

(defnc search-states [states]
  (= (count states) 0) (:flow @best-state)
  :let [state (peek states), states (pop states)]
  (complete? state) (do (swap! best-state (fn [b s] (if (> (:flow s) (:flow b)) s b)) state) (recur states))
  (seen? state) (recur states)
  (< (upper-bound state) (:flow @best-state)) (recur states)
  (recur (into states (next-states state))))

;; (defn print-path [path]
;;   (doseq [[index [step target]] (med/indexed path)]
;;     (cond
;;       (= step :move) (println "Move" target)
;;       (= step :flip) (println "Flip" target (flow-benefit target (inc index))))))

(defnc solution-part1 [] (search-states [initial-state]))

;; Part 2

(defrecord StateElephant [cave1 cave2 valves time flow path])
(def initial-state-elephant (->StateElephant :AA :AA zero-valves 1 0 []))
(def best-state (atom (->StateElephant :AA :AA zero-valves 1 1828 [])))


(defnc check-state [{:keys [path flow] :as state}]
  :let [actual-flow
        (apply + (for [[index time-step] (med/indexed path)
                       [step target] time-step]
                   (if (= step :flip) (flow-benefit target (inc index)) 0)))]
  (= flow actual-flow))

(defnc ->stateElephant [& s]
  :let [state (apply ->StateElephant s)]
  (check-state state) state
  :else (println state))

(defnc next-states-elephant [{:keys [cave1 cave2 valves time flow path] :as state}]
  :let [adjacent-caves1 (map uber/dest (uber/out-edges cave-graph cave1)),
        adjacent-caves2 (map uber/dest (uber/out-edges cave-graph cave2))
        moves-to-adjacent-caves (vec (for [dest1 adjacent-caves1, dest2 adjacent-caves2]
                                       (->StateElephant dest1 dest2 valves (inc time) flow (conj path [[:move dest1] [:move dest2]]))))
        flow-from-flipping1 (flow-benefit cave1 time)
        flow-from-flipping2 (flow-benefit cave2 time),
        flip-cave1-move-cave2 (for [dest2 adjacent-caves2]
                                (->StateElephant cave1 dest2 (conj valves cave1) (inc time) (+ flow flow-from-flipping1) (conj path [[:flip cave1] [:move dest2]]))),
        flip-cave2-move-cave1 (for [dest1 adjacent-caves1]
                                (->StateElephant dest1 cave2 (conj valves cave2) (inc time) (+ flow flow-from-flipping2) (conj path [[:move dest1] [:flip cave2]])))
        flip-both (->StateElephant cave1 cave2 (conj valves cave1 cave2) (inc time) (+ flow flow-from-flipping1 flow-from-flipping2) (conj path [[:flip cave1] [:flip cave2]]))]
  (and (valves cave1) (valves cave2)) moves-to-adjacent-caves
  (or (valves cave1) (= cave1 cave2)) (into moves-to-adjacent-caves flip-cave2-move-cave1)
  (valves cave2) (into moves-to-adjacent-caves flip-cave1-move-cave2)
  :else (conj (into moves-to-adjacent-caves (concat flip-cave2-move-cave1 flip-cave1-move-cave2)) flip-both))

(defnc add-to-best-flow-info-elephant [m [cave1 cave2 valves time flow]]
  (reduce (fn [m t] (transform [(keypath [(set [cave1 cave2]) valves t]) (nil->val -1)] #(max flow %) m)) m (range time *MAXTIME*)))

(defnc seen-elephant? [{:keys [cave1 cave2 valves flow time]}]
  :let [best-flow (@best-flow-info [(set [cave1 cave2]) valves time] -1)]
  (<= flow best-flow) true
  :do (swap! best-flow-info add-to-best-flow-info-elephant [cave1 cave2 valves time flow])
  false)

(defnc upper-bound-elephant [{:keys [cave1 cave2 valves time flow]}]
  (transduce (comp (remove valves) (map (fn [dest] (let [dist (min (distances [cave1 dest]) (distances [cave2 dest]))] (flow-benefit dest (+ time dist)))))) + flow all-caves))

(defnc search-states-elephant [states]
  (= (count states) 0) (println (:flow @best-state))
  :let [state (peek states), states (pop states)]
  (complete? state) (do (swap! best-state (fn [b s] (if (> (:flow s) (:flow b)) s b)) state) (recur states))
  (seen-elephant? state) (recur states)
  (< (upper-bound-elephant state) (:flow @best-state)) (recur states)
  (recur (into states (next-states-elephant state))))

(defn print-path [path]
  (doseq [[index time-step] (med/indexed path)]
    (doseq [[step target] time-step]
      (when (= step :move) (print "Move" target " "))
      (when (= step :flip) (print "Flip" target (flow-benefit target (inc index)) " ")))
    (newline)))

(defnc solution-part2 [] (binding [*MAXTIME* 26] (search-states-elephant [initial-state-elephant])))

(defnc check-path [path]
  :let [flow
        (apply + (for [[index time-step] (med/indexed path)
                       [step target] time-step]
                   (if (= step :flip) (flow-benefit target (inc index)) 0)))]
  :do (println flow)
  :do (doseq [[[[_ target-a1] [_ target-a2]] [[_ target-b1] [_ target-b2]]] (partition 2 1 path)]
        (println target-a1 target-a2 target-b1 target-b2)
        (when (not= target-a1 target-b1) (assert (uber/find-edges cave-graph target-a1 target-b1)))
        (when (not= target-a2 target-b2) (assert (uber/find-edges cave-graph target-a2 target-b2)))))
    
