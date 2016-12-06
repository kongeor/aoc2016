(ns aoc2016.day6
  (:require [aoc2016.util :refer [resource-lines]]))

(defn el-comp [[_ v1] [_ v2]]
  (compare v1 v2))

(defn most-freq-char [cs]
  (ffirst (sort el-comp (frequencies cs))))

(defn error-correct [in]
  (let [char-seqs (apply map vector (resource-lines in))]
    (map most-freq-char char-seqs)))

;; TODO implement mutli comp support
(comment
  (apply str (error-correct "ind6.txt")))
