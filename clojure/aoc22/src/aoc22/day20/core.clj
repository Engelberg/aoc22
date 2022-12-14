(ns aoc22.day20.core
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

(def input (mapv parse-long (str/split-lines (slurp "src/aoc22/day20/input.txt"))))

(defrecord Link [value next prev])

(defn circular-list [v]
  (let [links (mapv #(->Link % (atom nil) (atom nil)) v), n (count v)]
    (doseq [i (range (dec n))]
      (reset! (:next (links i)) (links (inc i))))
    (reset! (:next (links (dec n))) (links 0))
    (doseq [i (range 1 n)]
      (reset! (:prev (links i)) (links (dec i))))
    (reset! (:prev (links 0)) (links (dec n)))
    links))

(def sample [1 2 -3 3 -2 0 4])

(defnc get-link [link n dir]
  (zero? n) link
  (recur @(dir link) (dec n) dir))

(defn remove-link [link]
  (let [p @(:prev link),
        n @(:next link)]
    (reset! (:next p) n)
    (reset! (:prev n) p)))

(defn insert-link-after [link1 link2]
  (let [n @(:next link2)]
    (reset! (:next link2) link1)
    (reset! (:prev link1) link2)
    (reset! (:prev n) link1)
    (reset! (:next link1) n)))

(defnc move [links i]
  :let [link (links i),
        n (:value link),
        pos-n? (pos? n),
        n (mod (abs n) (dec (count links)))]
  (zero? n) links
  :do (remove-link link)
  :let [attachment-point (get-link @(:prev link) (abs n) (if pos-n? :next :prev))]
  (do (insert-link-after link attachment-point) links))

(defnc decrypt [links]
  (reduce move links (range (count links))))

(defnc find-value [links value]
  (first (for [i (range (count links)),
               :when (= (:value (links i)) value)]
           (links i))))

(defnc solution-part-1 []
  :let [links (decrypt (circular-list input)),
        start (find-value links 0),
        l1 (get-link start 1000 :next)
        l2 (get-link l1 1000 :next)
        l3 (get-link l2 1000 :next)]
  (apply + (map :value [l1 l2 l3])))

(defnc solution-part-2 []
  :let [links (circular-list (map #(* % 811589153) input))
        links (nth (iterate decrypt links) 10),
        start (find-value links 0),
        l1 (get-link start 1000 :next)
        l2 (get-link l1 1000 :next)
        l3 (get-link l2 1000 :next)]
  (apply + (map :value [l1 l2 l3])))

;; For debugging purposes

(defnc print-list [links]
  :let [starting-link (links 0)]
  :do (println (:value starting-link))
  (loop [link @(:next starting-link)]
    (cond
      (= link starting-link) nil
      :do (println (:value link))
      (recur @(:next link)))))

(defnc print-link-data [links]
  (doseq [i (range (count links))]
    (let [link (links i)]
      (println i (:value link) (:value @(:prev link)) (:value @(:next link))))))
