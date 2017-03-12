(ns levenshtein.core
  (:require [taoensso.tufte :as tufte :refer [defnp p profiled profile]]
            [clojure.java.io :as io])
  (:gen-class))

(defn split-evenly [s]
  (let [splt (split-at (quot (count s) 2) s)]
    (if (even? (count s))
      splt
      (let [[left right] splt
            middle (first right)]
        [(concat left [middle]) right]))))

(defn- first-diff-pos [s1 s2]
  (if (not= (count s1) (count s2))
    (throw (ex-info "must be same size" {}))
    (some (fn [[c1 c2 pos]]
            (when (not= c1 c2)
              pos))
          (map vector s1 s2 (range)))))

(defn- alignable-via-insert?* [s1 s2 pos]
  (let [x (nth s2 pos)
        s1' (take (count s1) (concat (take pos s1) [x] (drop pos s1)))]
    (= (seq s1') (seq s2))))

(defn alignable-via-insert? [left right]
  (let [pos (first-diff-pos left right)]
    (or (alignable-via-insert?* left right pos)
        (alignable-via-insert?* right left pos))))

(defn- alignable-via-removal?* [s1 s2 pos]
  (let [s1' (concat (take pos s1) (drop (inc pos) s1))
        s2' (drop-last 1 s2)]
    (= (seq s1') (seq s2'))))

(defn alignable-via-removal? [left right]
  (let [pos (first-diff-pos left right)]
    (or (alignable-via-removal?* left right pos)
        (alignable-via-removal?* right left pos))))

(defn- hamming-distance [s1 s2]
  (when (= (count s1) (count s2))
    (reduce + (map #(if (not= %1 %2) 1 0) s1 s2))))

(defn alignable-via-substitution? [left right]
  (<= (hamming-distance left right) 1))

(defn near-palindrome? [s]
  (let [[left right] (split-evenly s)
        right (reverse right)]
    (or (alignable-via-substitution? left right)
        (alignable-via-insert? left right)
        (alignable-via-removal? left right))))

(defn -main [& _]
  (->> (drop 1 (line-seq (io/reader *in*)))
       (map #(if (near-palindrome? %) "1" "0"))
       (apply str)
       (println)))
