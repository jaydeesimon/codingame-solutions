(ns gravity.elegant)

(defn transpose [m]
  (for [i (range (count (first m)))]
    (map #(nth % i) m)))

(defn -main [& args]
  (let [width (read) height (read) _ (read-line)]
    (->> (repeatedly height read-line)
         transpose
         (map sort)
         transpose
         reverse
         (map #(println (apply str %)))
         doall)))
