(ns temperatures.core
  (:gen-class))

(defn comparator' [x y]
  (let [xz (Math/abs (- 0 x))
        yz (Math/abs (- 0 y))]
    (if (= xz yz)
      (* -1 (compare x y))
      (compare xz yz))))

(defn -main [& args]
  (let [n (read) _ (read-line)]
    (if (zero? n)
      (println 0)
      (->> (repeatedly n read)
           (sort comparator')
           (first)
           (println)))))
