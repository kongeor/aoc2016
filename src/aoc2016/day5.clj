(ns aoc2016.day5
  (:require [digest :refer [md5]]
            [clojure.string :refer [starts-with?]]))

(defn digest-id [id start]
  (loop [i start]
    (let [in (str id i)
          hash (md5 in)]
      (if (starts-with? hash "00000")
        [hash i]
        (recur (inc i))))))


(defn break-password [id]
  (loop [i 1
         pass ""]
    (let [[hash i] (digest-id id i)
          c (. hash substring 5 6)
          pass (str pass c)]
      (if (= 3 (count pass))
        pass
        (recur (inc i) pass)))))

(break-password "ugkcyxxp")
