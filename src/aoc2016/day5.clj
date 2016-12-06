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

(comment
  (break-password "ugkcyxxp"))

(defn solved? [pass]
  (= (count (filter nil? pass)) 0))

(defn char->int [c]
  (try
    (Integer/parseInt (str c))
    (catch NumberFormatException e)))

(char->int \1)

(defn update-pass [pass c idx]
  (if-let [idx (char->int idx)]
    (if (< -1 idx 8)
      (assoc pass (char->int idx) (str c))
      pass)
    pass))

(update-pass [nil nil nil] \c \1)

(defn break-password-2 [id]
  (loop [i 1
         pass (vec (repeat 8 nil))]
    (let [[hash i] (digest-id id i)
          [c idx] (. hash substring 5 8)
          pass (update-pass pass c idx)]
      (if (solved? pass)
        pass
        (recur (inc i) pass)))))

(comment
  (break-password-2 "ugkcyxxp"))
