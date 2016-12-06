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

(digest-id "ugkcyxxp" 1)

(md5 "abc3231929")
(md5 "abc5017308")

(str "asfd" 1)
