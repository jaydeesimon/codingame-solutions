(ns simplify-selection-ranges.core
  (:gen-class))

(defn consecutive? [a b]
  (or (= (- b a) 1)
      (= (- a b) 1)))

(defn split-consecutive [xs]
  (loop [list xs
         groups []]
    (cond (not (seq list)) groups
          (nil? (last groups)) (recur (rest list) (conj groups (first list)))
          (consecutive? (last groups) (first list)) (recur (rest list) (conj groups (first list)))
          :else (recur list (conj groups nil)))))

(defn -main [& _]
  (->> (read-line)
       (read-string)
       (sort)
       (split-consecutive)
       (partition-by nil?)
       (filter (comp some? first) )
       (map #(if (>= (count %) 3)
              (str (first %) "-" (last %))
              (clojure.string/join "," %)))
       (clojure.string/join ",")
       (println)))
