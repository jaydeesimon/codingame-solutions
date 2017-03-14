(ns sandpiles.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn- in-bounds? [sandpile coord]
  (some? (get-in sandpile coord)))

(defn distribute-coord [sandpile coord]
  (if (>= (get-in sandpile coord) 4)
    (reduce (fn [sandpile [offset val-to-add]]
              (let [neighbor-coord (mapv + offset coord)]
                (if (in-bounds? sandpile neighbor-coord)
                  (update-in sandpile neighbor-coord #(+ % val-to-add))
                  sandpile)))
            sandpile
            [[[-1 0] 1] [[0 -1] 1] [[0 0] -4] [[0 1] 1] [[1 0] 1]])
    sandpile))

(defn distribute-step [sandpile]
  (let [size (count sandpile)
        heavy-square (->> (for [r (range size) c (range size)] [r c])
                          (some #(when (>= (get-in sandpile %) 4) %)))]
    (if heavy-square
      (distribute-coord sandpile heavy-square)
      sandpile)))

(defn add-sandpiles [s1 s2]
  (let [s3 (mapv #(mapv + %1 %2) s1 s2)]
    (->> (iterate distribute-step s3)
         (partition 2 1)
         (take-while (fn [[prev' next']]
                       (not= prev' next')))
         (last)
         (last))))

(defn parse-sandpiles [lines]
  (let [n (quot (count lines) 2)
        [frst scnd] (partition n lines)
        ->matrix (fn [digits]
                   (mapv #(mapv (fn [c] (Integer/parseInt (str c))) %) digits))]
    (map ->matrix [frst scnd])))

(defn -main [& _]
  (let [[s1 s2] (parse-sandpiles (drop 1 (line-seq (io/reader *in*))))]
    (->> (add-sandpiles s1 s2)
         (mapv #(println (apply str %))))))
