(ns number-of-letters-in-a-number.core
  (:gen-class))

(defn ->binary [n]
  (-> n str biginteger (.toString 2)))

(defn next-nolian [n]
  (->> (->binary n)
       (map #(if (= % \1) 3 4))
       (reduce +)))

(defn nolian [start]
  (iterate next-nolian start))

(defn -main [& _]
  (let [start (read)
        n (read)
        nolians (take 10 (nolian start))]
    (if (< n 10)
      (println (nth nolians n))
      (println (last nolians)))))
