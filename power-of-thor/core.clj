(ns power-of-thor.core
  (:gen-class))

(defn n-units [n unit]
  (repeat (Math/abs n) (mapv (partial * (Integer/signum n)) unit)))

(defn remainder [coll1 coll2]
  (if (>= (count coll1) (count coll2))
    (drop (count coll2) coll1)
    (drop (count coll1) coll2)))

(def directions {[-1 -1] "NW" [-1 0] "W" [-1 1] "SW" [0 -1] "N" [0 1] "S" [1 -1] "NE" [1 0] "E" [1 1] "SE"})

(defn path [start end]
  (let [[x y] (mapv - end start)
        x-units (n-units x [1 0])
        y-units (n-units y [0 1])]
    (as-> (map (partial mapv +) x-units y-units) t
          (concat t (remainder x-units y-units))
          (map directions t))))

(defn -main [& args]
  (let [end [(read) (read)]
        start [(read) (read)]
        path (path start end)]
    (loop [path path]
      (do (read)
          (println (first path))
          (recur (rest path))))))










