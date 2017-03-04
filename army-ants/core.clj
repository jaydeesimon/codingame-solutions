(ns army-ants.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn initialize-ant-group [ants direction]
  (map #(hash-map :name % :direction direction) ants))

(defn initialize-ants [ants-right ants-left]
  (concat (initialize-ant-group (reverse ants-right) :right)
          (initialize-ant-group ants-left :left )))

(defn- facing-each-other? [[{direction1 :direction} {direction2 :direction}]]
  (and (= direction1 :right) (= direction2 :left)))

(defn find-swaps [ants]
  (->> (partition 2 1 ants)
       (filter facing-each-other?)
       (reduce (fn [swaps [ant1 ant2]]
                 (-> swaps
                     (assoc ant1 ant2)
                     (assoc ant2 ant1)))
               {})))

(defn jump [ants]
  (let [swaps (find-swaps ants)]
    (map #(or (get swaps %) %) ants)))

(defn jump-seconds [ants-right ants-left seconds]
  (let [ants (initialize-ants ants-right ants-left)]
    (->> (last (take (inc seconds) (iterate jump ants)))
         (map :name)
         (apply str))))

(defn -main [& _]
  (let [[ants-right ants-left seconds] (->> (drop 1 (line-seq (io/reader *in*)))
                                            (map #(%1 %2) [identity identity read-string]))]
    (println (jump-seconds ants-right ants-left seconds))))
