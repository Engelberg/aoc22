(ns aoc22.day7.core
  (:refer-clojure :exclude [cond if-let if-some when-let when-some])
  (:require [better-cond.core :refer [cond if-let if-some when-let when-some defnc defnc-]]
            [instaparse.core :as insta]
            [com.rpl.specter :refer :all]))

(def input (slurp "src/aoc22/day7/input.txt"))

(def terminal-parser
  (insta/parser "<Shell> = Commands+
                 <Commands> = cd | ls
                 cd = <'$ cd'> #'\\S+'
                 ls = <'$ ls'> output
                 <output> = (dir | file)*
                 dir = <'dir'> #'\\S+'
                 file = #'[0-9]+' #'\\S+'" :auto-whitespace :standard))

(def parsed-input (insta/transform {:file (fn [size filename] [(parse-long size) filename])}
                                   (terminal-parser input)))

(defnc build-tree-from-command [[tree navigator] [command-type :as command]]
  (= command-type :cd) (cond
                         :let [dir (peek command)]
                         (= dir "/") [tree []]
                         (= dir "..") [tree (pop (pop navigator))]
                         :else [tree (conj navigator :dirs (peek command))])
  (= command-type :ls) (let [contents (rest command),
                             dirs (into {} (for [[x y] contents :when (= x :dir)] [y {}]))
                             files (filter (fn [[x y]] (number? x)) contents)]
                         [(setval navigator {:dirs dirs :files files} tree) navigator]))

(defnc build-tree [commands] (first (reduce build-tree-from-command [{} []] commands)))

(defnc add-size-information [{:keys [dirs files] :as tree}]
  :let [filesize (transduce (map first) + files),
        dirs-with-sizes (into {} (for [[dir contents] dirs] [dir (add-size-information contents)])),
        dirsizes (transduce (comp (map val) (map :size)) +  dirs-with-sizes)]
  (assoc tree :dirs dirs-with-sizes :size (+ filesize dirsizes)))

(defnc dir-seq [tree] (tree-seq #(seq (:dirs %)) #(vals (:dirs %)) tree))

(def directory-structure (add-size-information (build-tree parsed-input)))

(def solution-part1 (transduce (comp (map :size) (filter #(<= % 100000)))
                               + (dir-seq directory-structure)))

(def free-space (- 70000000 (:size directory-structure)))
(def needed-to-free (- 30000000 free-space))

(def solution-part2 (transduce (comp (map :size) (filter #(>= % needed-to-free)))
                               min Long/MAX_VALUE (dir-seq directory-structure)))
