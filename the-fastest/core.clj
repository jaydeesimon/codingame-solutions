(ns the-fastest.core
  (:gen-class))

(defn seconds [time]
  (->> (clojure.string/split time #":")
       (map #(Integer/valueOf %))
       (map * [3600 60 1])
       (reduce +)))

(defn -main [& _]
  (let [n (read) _ (read-line)]
    (println (apply min-key seconds (repeatedly n read-line)))))
