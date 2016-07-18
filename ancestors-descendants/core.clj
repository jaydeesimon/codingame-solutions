(ns ancestors-descendants.core
  (:gen-class))

(defn node [s]
  (let [[[_ dots content]] (re-seq #"(^\.*)(.*)" s)]
    {:level (count dots)
     :content content}))

(defn trees [lines]
  (let [nodes (mapv node lines)]
    (->> (map-indexed vector nodes)
         (filter (comp zero? :level second))
         (partition-all 2 1)
         (map (fn [[[start _] [end _]]]
                (let [range (if (some? end) [start end] [start])]
                  (apply subvec nodes range)))))))

(defn leaves [tree]
  (->> (partition-all 2 1 tree)
       (filter (fn [[{level-cur :level} {level-nxt :level}]]
                 (or (nil? level-nxt)
                     (<= level-nxt level-cur))))
       (map first)))

(defn path [tree node]
  (loop [level (dec (:level node))
         path [node]]
    (if (neg? level)
      (reverse path)
      (let [parent-candidates (->> (take-while (partial not= node) tree)
                                   (filter #(= (:level %) level)))]
        (recur (dec level)
               (conj path (last parent-candidates)))))))

(defn format-path [path]
  (clojure.string/join " > " (map :content path)))

(defn -main [& _]
  (let [n (read) _ (read-line)
        leaf-paths (for [tree (trees (repeatedly n read-line))
                         leaf (leaves tree)]
                     (path tree leaf))]
    (mapv (comp println format-path) leaf-paths)))
