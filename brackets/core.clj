(ns brackets.core
  (:gen-class))

(defn balanced? [expression]
  (loop [counts [0 0 0]
         expr expression]
    (if (or (nil? (first expr)) (some neg? counts))
      (every? zero? counts)
      (let [b (case (first expr)
                \{ [1 0 0]
                \} [-1 0 0]
                \[ [0 1 0]
                \] [0 -1 0]
                \( [0 0 1]
                \) [0 0 -1]
                [0 0 0])]
        (recur (mapv + counts b) (rest expr))))))

(defn -main [& args]
  (let [expression (read-line)]
    (println (balanced? expression))))


