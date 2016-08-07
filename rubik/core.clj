(ns rubik.core
  (:gen-class))

(defn visibile-cubes [n]
  (if (= n 1)
    1
    (+ 2 (* 6 (* (dec n) (dec n))))))

(defn -main [& _]
  (println (visibile-cubes (read))))
