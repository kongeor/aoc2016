(ns aoc2016.core
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

(def dir-map
  {:n {:R :e :L :w}
   :w {:R :n :L :s}
   :s {:R :w :L :e}
   :e {:R :s :L :n}})

(defn direction [current new-dir]
  (new-dir (current dir-map)))

;; pos is {:dir :N :h 0 :v 0}

(defn process-dir [pos mv]
  (let [d (first mv)
        s (string/join (rest mv))
        d (keyword (String/valueOf d))
        d (direction (:dir pos) d)
        pos (assoc pos :dir d)
        s (Integer/parseInt (String/valueOf s))]
    (condp = d
      :n (update-in pos [:v] + s)
      :w (update-in pos [:h] - s)
      :s (update-in pos [:v] - s)
      :e (update-in pos [:h] + s))))

(defn text->tokens
  ([] (text->tokens (slurp (io/resource "ind1.txt"))))
  ([in]
   (let [in (string/split in #",")
         in (map string/trim in)]
     in)))

(def initial {:dir :n :h 0 :v 0})


;; first solution
(reduce #(process-dir %1 %2) initial (text->tokens))

(defn p2-data [tokens]
  (reduce #(let [new-dir (process-dir (last %1) %2)]
             (conj %1 new-dir)) (vector initial) tokens))

(def data (reduce #(let [new-dir (process-dir (last %1) %2)]
           (conj %1 new-dir)) (vector initial) (text->tokens)))

(map-indexed (fn [i e]) data)


(defn p2 [data]
  (loop [d data]
    (let [[x & xs] d]
      (if (some #{x} xs)
        x
        (recur (vec xs))))))

(defn -range [x y]
  (let [f (if (> x y) dec inc)]
    (loop [r [x]
           x x]
      (let [x (f x)]
        (if (= x y)
          r
          (recur (conj r x) x))))))

(defn path [[from to]]
  (let [hor-changed? (not= (:h from) (:h to))
        ver-changed? (not= (:v from) (:v to))
        fhor (:h from)
        fver (:v from)
        thor (:h to)
        tver (:v to)]
    (if hor-changed?
      (mapv (fn [i] {:v fver :h i}) (-range fhor thor))
      (mapv (fn [i] {:v i :h fhor}) (-range fver tver)))))

(-range -7 -5)

(mapcat path (partition 2 1 (p2-data (text->tokens "R8, R4, R4, R8"))))

;; second solution

(p2 (mapcat path (partition 2 1 data)))

(p2 data)
(vector '(1 2 3))
(let [[x & xs] [1 2 3]]
  xs)
(some #{4} [1 2 3])
(frequencies (map #(dissoc % :dir) data))
