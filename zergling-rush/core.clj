(ns zergling-rush.core
  (:require [zergling-rush.util :as util]
            [clojure.java.io :as io])
  (:gen-class))

(defn board-dims [board]
  [(count board) (count (first board))])

(defn neighbors [board directions coord]
  (->> (map #(mapv + coord %) directions)
       (filter (fn [coord']
                 (= (get-in board coord') \.)))))

(defn edge? [board coord]
  (let [[rows cols] (board-dims board)
        [row col] coord]
    (or (zero? row) (zero? col) (= (dec rows) row) (= (dec cols) col))))

(defn bfs-path-to-edge? [board start]
  (let [movable-directions [[1 0] [-1 0] [0 1] [0 -1]]]
    (loop [frontier [start]
           visited #{start}]
      (let [[current & frontier] frontier
            unvisited-neighbors (->> current
                                     (neighbors board movable-directions)
                                     (remove visited))]
        (cond (nil? current) false
              (edge? board current) true
              :else (recur (into (vec frontier) unvisited-neighbors)
                           (into visited [current])))))))

(defn base-coordinates [board]
  (let [[rows cols] (board-dims board)]
    (->> (for [r (range rows) c (range cols)] [r c])
         (filter (fn [coord]
                   (= (get-in board coord) \B))))))

(defn zergling-coords [board]
  (let [surrounding-directions [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]]
    (->> (base-coordinates board)
         (mapcat #(neighbors board surrounding-directions %))
         (filter #(bfs-path-to-edge? board %)))))

(defn assoc-coords [board coords x]
  (reduce (fn [board coord]
            (assoc-in board coord x))
          board
          coords))

(defn print-board! [board]
  (doseq [row board]
    (println (apply str row))))

(defn -main [& _]
  (let [board (->> (line-seq (io/reader *in*))
                   (drop 1)
                   (mapv vec))
        zergling-coords (zergling-coords board)
        board (assoc-coords board zergling-coords \z)]
    (print-board! board)))
