(ns aoc2016.day4
  (:require [clojure.java.io :as io]))

(def data (clojure.string/split (slurp (io/resource "ind4.txt")) #"\n"))

(defn parse [in]
  (re-find #"([\w|-]+)-(\d+)\[(\w+)\]$" in))

(defn el-comp [[k1 v1] [k2 v2]]
  (if (= v1 v2)
    (compare k1 k2)
    (compare v2 v1)))

(map first (sort el-comp {:a 1 :b 2 :c 2}))

(defn verify [entry]
  (let [[_ & [encrypted sector checksum]] (parse entry)
        parsed (->> (frequencies (remove #(= \- %) (seq encrypted)))
                    (sort el-comp)
                    (map first)
                    (take 5))]
    (when (= (seq checksum) parsed)
      [encrypted (Integer/parseInt sector)])))

;; FIXME
(reduce + (remove nil? (map verify data)))

(int \a)
(char 99)

(defn decrypt-char [c shift]
  (condp = c
    \space \space
    \- \space
    (char (+  (mod (+ (- (int c) 97) shift) 26) 97))))

(decrypt-char \- 53)

(defn decrypt-entry [[encrypted sector]]
  [(apply str (map #(decrypt-char % sector) (seq encrypted)))
   sector])

(defn decrypt-data [entries]
  (map decrypt-entry entries))

(clojure.pprint/pprint
   (filter #(clojure.string/includes? % "north") (decrypt-data (remove nil? (map verify data)))))
