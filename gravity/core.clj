(ns gravity.core
  (:require [clojure.pprint :as pp])
  (:gen-class))

(defn take-until
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

(defn dims [grid]
  [(count grid) (count (first grid))])

(defn coordinates [[rows cols]]
  (for [r (range rows)
        c (range cols)]
    [r c]))

(defn move-down? [grid coord]
  (and (= (get-in grid coord) \#)
       (= (get-in grid (mapv + coord [1 0])) \.)))

(defn move-down [grid coord]
  (-> grid
      (assoc-in coord \.)
      (assoc-in (mapv + coord [1 0]) \#)))

(defn step [grid]
  (reduce (fn [grid coord]
            (if (move-down? grid coord)
              (move-down grid coord)
              grid))
          grid
          (coordinates (dims grid))))

(defn done? [grid]
  (every? #(not (move-down? grid %)) (coordinates (dims grid))))

(defn -main [& _]
  (let [_ (read) rows (read) _ (read-line)]
    (->> (mapv vec (repeatedly rows read-line))
         (iterate step)
         (take-until done?)
         (last)
         (map clojure.string/join)
         (clojure.string/join \newline)
         (println))))
