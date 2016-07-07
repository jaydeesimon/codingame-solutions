(ns Solution
  (:gen-class))

;; magaiti's solution for reference

(defn nth-letter? [n ch]
  (contains? (set (take-nth n "$ABCDEFGHIJKLMNOPQRSTUVWXYZ")) ch))

(defn scramble [s pred f]
  (let [A (filter (comp pred second) (map-indexed vector s))
        A (map vector (map first A) (f (map second A)))]
    (apply str (reduce #(apply assoc %1 %2) (vec s) A))))

(defn -main [& args]
  (-> (read-line)
      (reverse)
      (scramble #(not= \space %) reverse)
      (scramble #(nth-letter? 4 %) #(concat (take-last 1 %) (drop-last 1 %)))
      (scramble #(nth-letter? 3 %) #(concat (drop 1 %) (take 1 %)))
      (scramble #(nth-letter? 2 %) reverse)
      (println)))
