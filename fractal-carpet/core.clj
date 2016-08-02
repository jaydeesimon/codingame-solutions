(ns fractal-carpet.core
  (:gen-class))

(defn base3 [n]
  (.toString (BigInteger. (str n)) 3))

;; From https://en.wikipedia.org/wiki/Sierpinski_carpet
;; "It can be realised as the set of points in the unit
;; square whose coordinates written in base three do not
;; both have a digit '1' in the same position.
(defn hole? [[x y]]
  (let [f (comp reverse base3)]
    (->> (map vector (f x) (f y))
         (some (partial = [\1 \1])))))

(defn coordinates [[x1 y1] [x2 y2]]
  (for [y (range y1 (inc y2))
        x (range x1 (inc x2))]
    [x y]))

(defn patch [[x1 y1] [x2 y2]]
  (->> (coordinates [x1 y1] [x2 y2])
       (map #(if (hole? %) \+ \0))
       (partition (inc (- x2 x1)))
       (map (partial apply str))
       (clojure.string/join \newline)))

(defn -main [& _]
  (let [_ (read) x1 (read) y1 (read) x2 (read) y2 (read)]
    (println (patch [x1 y1] [x2 y2]))))
