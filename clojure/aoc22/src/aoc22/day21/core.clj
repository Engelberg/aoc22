(ns aoc22.day20.core
  (:refer-clojure :exclude [cond if-let if-some when-let when-some])
  (:require [better-cond.core :refer [cond if-let if-some when-let when-some defnc defnc-]]            
            [clojure.string :as str]
            [ubergraph.core :as uber]))

(def input (mapv #(str/split % #":?\s")(str/split-lines (slurp "src/aoc22/day21/input.txt"))))
(def inits (apply concat 
                  (for [[v1 v2 v3 v4 :as line] input]
                    (cond
                      :let [[v1 v2 v3 v4] (if (= (count line) 4)
                                            [(keyword v1) (keyword v2) (eval (read-string v3)) (keyword v4)]
                                            [(keyword v1) (parse-long v2) nil nil])]
                      (nil? v3) [[v1 {:val v2}]]
                      :else [[v1 {:op v3}] [v1 v2 {:arg 1}] [v1 v4 {:arg 2}]]))))
(def g (apply uber/digraph inits))

(defnc get-equation [g node]
  :let [{dest1 :dest} (first (uber/find-edges g {:src node, :arg 1}))
        {dest2 :dest} (first (uber/find-edges g {:src node, :arg 2}))
        val1 (uber/attr g dest1 :val), val2 (uber/attr g dest2 :val)
        op (uber/attr g node :op)]
  [op dest1 dest2 val1 val2])

(defnc compute-values [g node]
  (not (uber/attr g node :op)) g
  :let [[op dest1 dest2 val1 val2] (get-equation g node)]
  (and val1 val2) (uber/add-attr g node :val (op val1 val2))
  :else g)
  
(defnc solution-part-1 []
  :let [g1 (reduce compute-values g (alg/post-traverse g))]
  (uber/attr g1 :root :val))

(defnc constrain-node
  ([g] :let [[op dest1 dest2 val1 val2] (get-equation g :root)]
   (nil? val1) (constrain-node g dest1 val2) (constrain-node g dest2 val1))
  ([g n v]
   (= n :humn) v
   :let [[op dest1 dest2 val1 val2] (get-equation g n)]
   (and (= op +) (nil? val1)) (recur g dest1 (- v val2))
   (and (= op +) (nil? val2)) (recur g dest2 (- v val1))
   (and (= op *) (nil? val1)) (recur g dest1 (/ v val2))
   (and (= op *) (nil? val2)) (recur g dest2 (/ v val1))
   (and (= op -) (nil? val1)) (recur g dest1 (+ val2 v))
   (and (= op -) (nil? val2)) (recur g dest2 (- val1 v))
   (and (= op /) (nil? val1)) (recur g dest1 (* val2 v))
   (and (= op /) (nil? val2)) (recur g dest2 (/ val1 v))))

(defnc solution-part-2 []
  :let [g2 (reduce compute-values (uber/remove-attr g :humn :val) (alg/post-traverse g))]
  (constrain-node g2))



