(ns vortex.core
  (:gen-class))

(defn width [matrix]
  (count (first matrix)))

(defn height [matrix]
  (count matrix))

(defn rotate-cw [matrix]
  (apply mapv (comp vec rseq vector) matrix))

(defn rotate-ccw [matrix]
  (apply mapv vector (map rseq matrix)))

(defn outer-ring [matrix]
  (let [left (drop-last (reverse (first (rotate-cw matrix))))
        bottom (drop-last (last matrix))
        right (drop-last (reverse (first (rotate-ccw matrix))))
        top (drop-last (reverse (first matrix)))]
    (concat left bottom right top)))

(defn drop-outer-ring [matrix]
  (->> (drop 1 matrix)
       (drop-last 1)
       (map (comp vec (partial drop 1)))
       (mapv (comp vec (partial drop-last 1)))))

(defn inner-matrices [matrix]
  (take-while (comp not empty?) (iterate drop-outer-ring matrix)))

(defn outer-rings [matrix]
  (let [multi-dim-matrices (filter #(> (count %) 1) (inner-matrices matrix))]
    (take-while (comp not empty?) (map outer-ring multi-dim-matrices))))

(defn shift-right [n coll]
  (take (count coll) (drop (- (count coll) (rem n (count coll))) (cycle coll))))

(defn coordinate-matrix [width height]
  (mapv vec (partition width (for [h (range height) w (range width)] [h w]))))

(defn crank [x matrix]
  (->> (mapcat (partial shift-right x) (outer-rings matrix))
       (map vector (mapcat identity (outer-rings (coordinate-matrix (width matrix) (height matrix)))))
       (reduce #(assoc-in %1 (first %2) (second %2)) matrix)))

(defn -main [& _]
  (let [w (read) h (read) x (read)]
    (->> (mapv vec (partition w (repeatedly (* w h) read)))
         (crank x)
         (map (partial clojure.string/join " "))
         (mapv println))))
