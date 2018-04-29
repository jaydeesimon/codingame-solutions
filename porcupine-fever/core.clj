(ns porcupine-fever.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.java.io :as io])
  (:import (java.io Reader)))

;; wish this existed in clojure.core
(defn take-until
  [pred coll]
  (lazy-seq
    (when-let [coll (seq coll)]
      (let [x (first coll)]
        (cons x (when-not (pred x)
                  (take-until pred (rest coll))))))))


(defn next-cage-state [{:keys [sick healthy]}]
  (let [potential-sick (* sick 2)
        new-healthy (max (- healthy potential-sick) 0)]
    {:sick    (if (>= potential-sick healthy)
                healthy
                potential-sick)
     :healthy new-healthy}))

(defn parse-input [s]
  (let [s (if (instance? Reader s) (slurp s) s)
        [_ years & rst] (->> (str/split s #"\s")
                             (map #(Integer/parseInt %)))]
    {:years years
     :cages (map (fn [[sick healthy _]]
                   {:sick sick :healthy healthy})
                 (partition 3 rst))}))

(defn alive-per-year [{:keys [cages years]}]
  (->> (iterate #(map next-cage-state %) cages)
       (drop 1)
       (take years)
       (map (fn [cages]
              (reduce (fn [alive {:keys [sick healthy]}]
                        (+ sick healthy alive))
                      0
                      cages)))
       (take-until zero?)))

(defn -main [& _]
  (doseq [alive (alive-per-year (parse-input (io/reader *in*)))]
    (println alive)))

