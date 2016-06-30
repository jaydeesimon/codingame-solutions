(ns snake-encoding.core
  (:require [clojure.string :as str])
  (:import (java.io BufferedReader))
  (:gen-class))

(defn top? [[r _]]
  (zero? r))

(defn bottom? [[r _] n]
  (= r (dec n)))

(defn shift-right [[r c]]
  [r (inc c)])

(defn shift-up [[r c]]
  [(dec r) c])

(defn shift-down [[r c]]
  [(inc r) c])

(defn upper-right? [[r c] n]
  (and (zero? r) (= c (dec n))))

(defn translate [n [_ c :as coord]]
  (cond (upper-right? coord n) [(dec n) 0]
        (and (top? coord) (even? c)) (shift-right coord)
        (and (bottom? coord n) (odd? c)) (shift-right coord)
        (even? c) (shift-up coord)
        (odd? c) (shift-down coord)
        :else (throw (ex-info "missed a case" {}))))

(defn translation-coords [n]
  (let [coords (for [r (range n)
                     c (range n)]
                 [r c])]
    (reduce (fn [m coord]
              (assoc m coord (translate n coord))) {} coords)))

(defn step [snake]
  (let [translation-coords (translation-coords (count snake))]
    (reduce (fn [snake' coord]
              (let [tc (get translation-coords coord)]
                (assoc-in snake' tc (get-in snake coord))))
            snake
            (keys translation-coords))))

(defn step-n [snake n]
  (reduce (fn [snake _] (step snake)) snake (range n)))

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
    (doseq [ls (step-n snake x)]
      (println (str/join ls)))))

