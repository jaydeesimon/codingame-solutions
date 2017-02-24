(ns kolakoski.core
  (:gen-class))

(defn kolakoski [s0 s1 limit]
  (take limit (loop [idx 0
                     sek []
                     active-s s0]
                (if (> (count sek) limit)
                  sek
                  (let [n (or (get sek idx) active-s)]
                    (recur (inc idx)
                           (apply (partial conj sek) (repeat n active-s))
                           (if (= active-s s0) s1 s0)))))))

(defn -main [& _]
  (let [limit (read)
        _ (read-line)
        s0 (read)
        s1 (read)]
    (println (apply str (kolakoski s0 s1 limit)))))
