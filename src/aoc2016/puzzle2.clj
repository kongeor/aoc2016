(ns aoc2016.puzzle2
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def key-map
  {1 {:U 1 :L 1 :R 2 :D 4}
   2 {:U 2 :L 1 :R 3 :D 5}
   3 {:U 3 :L 2 :R 3 :D 6}
   4 {:U 1 :L 4 :R 5 :D 7}
   5 {:U 2 :L 4 :R 6 :D 8}
   6 {:U 3 :L 5 :R 6 :D 9}
   7 {:U 4 :L 7 :R 8 :D 7}
   8 {:U 5 :L 7 :R 9 :D 8}
   9 {:U 6 :L 8 :R 9 :D 9}})

(def data (string/split (slurp (io/resource "ind2.txt")) #"\n"))

(defn ->key [current direction]
  (direction (key-map current)))

(defn char->keyword [c]
  (keyword (String/valueOf c)))

(defn process [in current]
  (let [in (mapv char->keyword (seq in))]
    (reduce #(->key %1 %2) current in)))

(defn process-all [in current]
  (reduce #(conj %1 (process %2 (last %1))) [current] in))

(process-all data 5)

(def _ nil)
(def data2 [[_ _ 1 _ _]
            [_ 2 3 4 _]
            [5 6 7 8 9]
            [_ "A" "B" "C" _]
            [_  _  "D"  _  _]])

(defn char-at-loc [{:keys [x y]}]
  (nth (nth data2 y nil) x nil))

(defn move [{:keys [x y] :as pos} dir]
  (let [dir (char->keyword dir)
        new-pos (condp = dir
                  :U (update-in pos [:y] - 1)
                  :D (update-in pos [:y] + 1)
                  :R (update-in pos [:x] + 1)
                  :L (update-in pos [:x] - 1))]
    (if (char-at-loc new-pos)
      new-pos
      pos)))

(map char-at-loc
     (reduce
       (fn [positions in]
         (conj positions (reduce #(move %1 %2) (last positions) (seq in))))
       [{:x 0 :y 2}] data))

(move {:x 0 :y 2} \R)
