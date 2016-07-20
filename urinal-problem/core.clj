(ns urinal-problem.core
  (:gen-class))

(defn abs [n]
  (Math/abs n))

(defn update' [m k f]
  (assoc m k (f (get m k))))

;; if it borders a wall, then the entire distance
;; is usable, if it borders an occupied urinal,
;; only half the distance is usable
(defn usable-distance [start end border]
  (let [distance (dec (- end start))]
    (if (= border \W)
      distance
      (int (Math/ceil (/ distance 2))))))

(defn optimal-urinal-index [urinals]
  (->> (map-indexed vector (concat [\W] urinals [\W]))
       (map #(update' % 0 dec))
       (filter #(or (= (second %) \W) (= (second %) \!)))
       (partition 3 1)
       (map (fn [[[left-idx left-border] [idx _] [right-idx right-border]]]
              [idx (* -1 (usable-distance left-idx idx left-border)) (usable-distance idx right-idx right-border)]))
       (map (fn [[idx left right]]
              [idx (max-key abs left right)]))
       (sort-by (juxt first second) (fn [[idx dist] [idx' dist']]
                                      (if (= (abs dist) (abs dist'))
                                        (compare idx idx')
                                        (compare (abs dist') (abs dist)))))
       (first)
       (apply +)))

(defn -main [& args]
  (let [_ (read-line) urinals (read-line)]
    (println (optimal-urinal-index urinals))))

;; calculates the distance between
;; the "this" urinal and the next
;; occupied or the size of the whole
;; thing if it's a wall
(defn dist0 [s N]
  (case (first s)
    nil N
    \!  0
    (inc (dist0 (rest s) N))))

;; for an index, calculates the minimum
;; distance between the left and the right
;; of this index
(defn dist [i]
  (min (dist0 (drop i S))
       (dist0 (reverse (take (inc i) S)))))

;; for every index, calculate the largest distance
(println (apply max-key dist (range N)))

#_(defn -main [& args]
  (let [N (read) _ (read-line) S (read-line)]

    ;; gives you the distance between
    (defn dist0 [s]
      (case (first s)
        nil N
        \!  0
        (inc (dist0 (rest s)))))


    (defn dist [i]
      (min (dist0 (drop i S))
           (dist0 (reverse (take (inc i) S)))))

    (println (apply max-key dist (range N)))))
