(ns aoc2016.puzzle3
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def data (string/split (slurp (io/resource "ind3.txt")) #"\n"))

(defn str->int [s]
  (Integer/parseInt s))

(defn parse-tri [s]
  (mapv str->int (string/split (string/trim s) #"\s+")))

(defn tri-comb [[a b c]]
  [[a b c]
   [a c b]
   [b c a]])

(defn valid-lens [[a b c]]
  (> (+ a b) c))

(defn valid-comb [combos]
  (every? valid-lens combos))

(def tri-pred? (comp valid-comb tri-comb parse-tri))

(count (filter tri-pred? data))

(def data2 (partition 3 (apply mapcat vector (map parse-tri data))))

(partition 3 [1 2 3 4 5 6])

(defn hundred [n]
  (int (/ n 100)))

(defn same-hun-digit? [tri]
  (apply = (map hundred tri)))

(count (filter (comp valid-comb tri-comb) data2))
