(ns aoc22.day22.core
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
            [clojure.java.io :as io]))

(defnc irange [x y] (range x (inc y)))
(def v+ (partial mapv +))
(def east [0 1]) (def west [0 -1]) (def north [-1 0]) (def south [1 0])
(def right {north east, east south, south west, west north})
(def left {north west, west south, south east, east north})
(def facing {east 0, south 1, west 2, north 3})

(let [[m i] (str/split (slurp "src/aoc22/day22/input.txt") #"\n\n|\r\n\r\n")]
  (def grid (into {} (for [[row line] (med/indexed (str/split-lines m)),
                           :let [max-col (count line)],
                           col (range max-col),
                           :let [character (nth line col)]
                           :when (not= character \space)]
                       [[row col] (nth line col)])))
  (def instructions (for [ins (re-seq #"[0-9]+|[LR]" i)]
                      (if (#{"L" "R"} ins) (keyword ins) (parse-long ins)))))

(let [by-row (map (fn [[row col]] {row col}) (keys grid))
      by-col (map (fn [[row col]] {col row}) (keys grid))]
  (def row-min (apply merge-with min by-row))
  (def row-max (apply merge-with max by-row))
  (def col-min (apply merge-with min by-col))
  (def col-max (apply merge-with max by-col)))

(def initial-state [[0 (row-min 0)] [0 1]])

(defnc wrap [[row col] dir]
  (= dir east) [[row (row-min row)] dir]
  (= dir west) [[row (row-max row)] dir]
  (= dir north) [[(col-max col) col] dir]
  (= dir south) [[(col-min col) col] dir])

(defnc move-one [wrap-rule [pos dir :as state]]
  :let [new-pos (v+ pos dir), cell (grid new-pos),
        [new-pos new-dir] (if (nil? cell) (wrap-rule pos dir) [new-pos dir]), cell (grid new-pos)]  
  (= cell \#) state
  [new-pos new-dir])

(defnc turn-right [[pos dir]] [pos (right dir)])
(defnc turn-left [[pos dir]] [pos (left dir)]) 

(defnc process-instruction [wrap-rule state ins]
  (= :L ins) (turn-left state)
  (= :R ins) (turn-right state)
  :else (nth (iterate (partial move-one wrap-rule) state) ins))

(defnc simulate [wrap-rule state instructions]
  (reduce (partial process-instruction wrap-rule) state instructions))

(defnc solution-part-1 []
  :let [[[row col] dir] (simulate wrap initial-state instructions)]
  :do (println [[row col] dir])
  (+ (* 1000 (inc row)) (* 4 (inc col)) (facing dir)))

(defnc cube-face [[row col]]
  (and (<= 0 row 49) (<= 100 col 149)) 1
  (and (<= 0 row 49) (<= 50 col 99)) 2
  (and (<= 50 row 99) (<= 50 col 99)) 3
  (and (<= 100 row 149) (<= 50 col 99)) 4
  (and (<= 100 row 149) (<= 0 col 49)) 5
  (and (<= 150 row 199) (<= 0 col 49)) 6
  :else (throw (ex-info "Invalid cube face" {:coords [row col]})))

(defnc wrap-3d [[row col :as pos] dir]
  :let [face (cube-face pos)]
  (and (= face 1) (= dir north)) [[199 (- col 100)] north]
  (and (= face 1) (= dir east)) [[(- 149 row) 99] west]
  (and (= face 1) (= dir south)) [[(- col 50) 99] west]
  (and (= face 2) (= dir north)) [[(+ 100 col) 0] east]
  (and (= face 2) (= dir west)) [[(- 149 row) 0] east]
  (and (= face 3) (= dir east)) [[49 (+ 50 row)] north]
  (and (= face 3) (= dir west)) [[100 (- row 50)] south]
  (and (= face 4) (= dir east)) [[(- 149 row) 149] west]
  (and (= face 4) (= dir south)) [[(+ 100 col) 49] west]
  (and (= face 5) (= dir north)) [[(+ 50 col) 50] east]
  (and (= face 5) (= dir west)) [[(- 149 row) 50] east]
  (and (= face 6) (= dir east)) [[149 (- row 100)] north]
  (and (= face 6) (= dir south)) [[0 (+ col 100)] south]
  (and (= face 6) (= dir west)) [[0 (- row 100)] south])

(defnc solution-part-2 []
  :let [[[row col] dir] (simulate wrap-3d initial-state instructions)]
  :do (println [[row col] dir])
  (+ (* 1000 (inc row)) (* 4 (inc col)) (facing dir)))
