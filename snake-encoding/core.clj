(ns snake-encoding.core
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn circular-shift-right [n coll]
  (concat (take-last n coll) (drop-last n coll)))

(defn column [table c]
  (mapv #(get % c) table))

(defn table->snake-seq [table]
  (flatten (reduce (fn [s i]
                     (if (even? i)
                       (conj s (reverse (column table i)))
                       (conj s (column table i))))
                   []
                   (range (count table)))))

(defn snake-seq->table [snake-seq]
  (let [n (int (Math/sqrt (count snake-seq)))
        partitioned (map-indexed (fn [i p]
                                   (vec (if (even? i)
                                          (reverse p)
                                          p)))
                                 (partition n snake-seq))]
    (reduce (fn [table i]
              (conj table (mapv #(get % i) partitioned)))
            []
            (range n))))

(defn lines->snake [lines]
  (reduce (fn [v line]
            (if (not (str/blank? line))
              (conj v (vec line))
              v))
          []
          lines))

(defn -main [& args]
  (let [n (read)
        x (read)
        snake (lines->snake (line-seq (BufferedReader. *in*)))]
    (doseq [ls (->> snake
                    table->snake-seq
                    (circular-shift-right x)
                    snake-seq->table)]
      (println (str/join ls)))))

