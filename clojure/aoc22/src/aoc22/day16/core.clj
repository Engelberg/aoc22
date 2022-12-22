(ns aoc22.day16.core
  (:refer-clojure :exclude [cond if-let if-some when-let when-some])
  (:require [better-cond.core :refer [cond if-let if-some when-let when-some defnc defnc-]]
            [clojure.string :as str]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg]
            [instaparse.core :as insta]))

(def input-parser (insta/parser "S = <'Valve '> Valve <' has flow rate='> Num <'; '> <#'\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+'> Valves
                                 Valve = #'[A-Z][A-Z]'
                                 Valves = Valve (<', '> Valve)*
                                 Num = #'[0-9]+'"))

(def input (for [l (str/split-lines (slurp "src/aoc22/day16/input.txt"))]
             (insta/transform {:Num parse-long, :Valve keyword, :Valves vector, :S vector}
                              (input-parser l))))
(def all-caves (mapv first input))

(def ^:dynamic *MAXTIME* 30)

(def valve-flow-map (into {} (for [[valve flow _] input] [valve flow])))
(def zero-valves (into #{} (for [[valve flow _] input :when (zero? flow)] valve)))
(def interesting-valves (into #{} (remove zero-valves) all-caves))

(def cave-graph (apply uber/digraph (for [[cave1 _ caves] input, cave2 caves] [cave1 cave2])))

(def distances (into {} (for [cave all-caves,
                              {:keys [cost end]} (alg/shortest-path cave-graph {:start-node cave :traverse true})]
                          [[cave end] cost])))

(defnc flow-benefit [cave time]
  (>= time *MAXTIME*) 0
  (* (- *MAXTIME* time) (valve-flow-map cave)))

(defrecord State [cave valves time flow path])
(def initial-state (->State :AA interesting-valves 1 0 []))

(defnc complete? [{:keys [time valves]}] (or (empty? valves) (>= time *MAXTIME*)))
(defnc next-states [{:keys [cave valves time flow path] :as state}]
  :let [destinations
        (for [valve valves,
              :let [dist (distances [cave valve]),
                    flow-from-flipping (flow-benefit valve (+ time dist))]
              :when (pos? flow-from-flipping)]
          (->State valve (disj valves valve) (inc (+ time dist)) (+ flow flow-from-flipping) (conj path [valve (inc (+ time dist))])))]
  (empty? destinations) [(->State cave valves *MAXTIME* flow path)]
  destinations)

(defnc upper-bound [{:keys [cave valves time flow]}]
  (transduce (comp (map (fn [dest] (let [dist (distances [cave dest])] (flow-benefit dest (+ time dist))))))
             + flow valves))

(defnc search-states [states best-state]
  (= (count states) 0) best-state
  :let [state (peek states), states (pop states)]
  (complete? state) (recur states (if (> (:flow state) (:flow best-state)) state best-state))
  (< (upper-bound state) (:flow best-state)) (recur states best-state)
  (recur (into states (next-states state)) best-state))

(defnc solution-part-1 [] (:flow (search-states [initial-state] initial-state)))

;; Part 2

(defnc all-completed-states "Prints answer quickly, proves optimality in 4s" [states completed-states]
  (= (count states) 0) (vals (apply merge-with (partial max-key :flow) completed-states))
  :let [state (peek states), states (pop states)]
  (complete? state) (recur states (conj completed-states {(:valves state) state}))
  (recur (into states (next-states state)) completed-states))

(defnc solution-part-2 []
  (binding [*MAXTIME* 26]
    (loop [human-states (seq (sort-by :flow > (all-completed-states [initial-state] []))),
           best-elephant-state {:flow 0}]
      (cond
        (nil? human-states) (:flow best-elephant-state)
        :let [{:keys [flow valves] :as state} (first human-states), human-states (next human-states),
              elephant-state (search-states [(->State :AA valves 1 flow [])] best-elephant-state)]
        (recur human-states (if (> (:flow elephant-state) (:flow best-elephant-state))
                              (do (println (:flow elephant-state)) elephant-state)
                              best-elephant-state))))))
