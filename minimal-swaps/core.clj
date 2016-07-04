(ns minimal-swaps.core
  (:gen-class))

(defn done? [xs]
  (let [partitioned (partition-by zero? xs)]
    (or (<= (count partitioned) 1)
        (and (= (count partitioned) 2)
             (= (ffirst partitioned) 1)))))

(defn swap [v i j]
  (let [iv (get v i)
        jv (get v j)]
    (-> v
        (assoc i jv)
        (assoc j iv))))

(defn rightmost [v i]
  (first (keep #(when (= (get v %) i) %)
               (range (dec (count v)) (dec 0) -1))))

(defn leftmost [v i]
  (first (keep #(when (= (get v %) i) %)
               (range (count v)))))

(defn count-swaps [v]
  (loop [v' v
         cnt 0]
    (if (done? v')
      cnt
      (let [lmz (leftmost v' 0)
            rmo (rightmost v' 1)]
        (recur (swap v' lmz rmo) (inc cnt))))))

#_(defn -main [& args]
  (let [_ (read-line)
        v (mapv read-string (clojure.string/split (read-line) #" "))]
    (println (count-swaps v))))

(defn -main [& args]
  (let [N (read)
        S (repeatedly N read)
        C (count (filter #{1} S))
        _ (println C)
        R (count (filter #{0} (take C S)))]
    (time (println R))))
