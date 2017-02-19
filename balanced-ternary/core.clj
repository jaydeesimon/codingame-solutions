(ns balanced-ternary.core
  (:gen-class))

(defn- permutations-n [n]
  (let [symbols (repeatedly n gensym)
        args (vec (interleave symbols (cycle [[-1 0 1]])))
        body (vec symbols)]
    (eval (list 'for args body))))

(defn permutations []
  (mapcat permutations-n (range 1 (inc 13))))

(defn pow [base exp]
  (apply * (repeat exp base)))

(defn evaluate [permutation]
  (->> (map (fn [digit exp]
              (* digit (pow 3 exp)))
            permutation (reverse (range (count permutation))))
       (reduce +)))

(defn encode [n]
  (->> (permutations)
       (some (fn [permutation]
               (when (= (evaluate permutation) n)
                 permutation)))))

(defn format-balanced-ternary [digits]
  (apply str (map #(if (neg? %) "T" (str %)) digits)))

(defn -main [& _]
  (println (format-balanced-ternary (encode (read)))))
