(ns aoc2016.util
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(defn resource-lines [filename]
  (string/split (slurp (io/resource filename)) #"\n"))
