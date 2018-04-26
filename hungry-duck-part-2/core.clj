(ns hungry-duck-two.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

(def memo (atom {}))

(defn max-food [m xy]
  (or (get @memo xy)
      (let [right (mapv + xy [0 1])
            move-right? (get-in m right)
            down (mapv + xy [1 0])
            move-down? (get-in m down)
            curr-val (get-in m xy)
            v (+ curr-val
                 (max
                   (if move-right? (max-food m right) 0)
                   (if move-down? (max-food m down) 0)))]
        (do
          (swap! memo #(assoc % xy v))
          v))))

(defn parse-input [source]
  (let [ns (as-> (slurp source) t
                 (str/split t #"\s")
                 (map #(Integer/parseInt %) t))
        n (first ns)]
    (->> (drop 2 ns)
         (partition n)
         (mapv vec))))

(defn -main [& _]
  (-> (parse-input (io/reader *in*))
      (max-food [0 0])
      (println)))
