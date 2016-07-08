(ns horse-racing-duals.core
  (:gen-class))

(defn -main [& args]
  (let [n (read)]
    (->> (repeatedly n read)
         (sort >)
         (partition 2 1)
         (map (partial apply -))
         (apply min)
         (println))))
