(ns magic-stones.core
  (:gen-class))

(defn reduce-stones [stones]
  (->> (sort stones)
       (partition-by identity)
       (mapcat #(partition-all 2 %))
       (map (fn [group]
              (if (= (count group) 2)
                (inc (first group))
                (first group))))))

(defn minimize-stones [stones]
  (loop [stones stones]
    (let [reduced-stones (reduce-stones stones)]
      (if (= stones reduced-stones)
        reduced-stones
        (recur reduced-stones)))))

(defn -main [& _]
  (let [num-stones (read)
        _          (read-line)
        stones     (repeatedly num-stones read)]
    (println (count (minimize-stones stones)))))
