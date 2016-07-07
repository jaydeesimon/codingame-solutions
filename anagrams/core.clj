(ns anagrams.core
  (:require [clojure.string :as str])
  (:gen-class))

(def alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn substitute [s pred substitutes]
  (apply str (first (reduce (fn [[acc substitutes] ltr]
                              (if (pred ltr)
                                [(conj acc (or (first substitutes) ltr)) (rest substitutes)]
                                [(conj acc ltr) substitutes]))
                            [[] substitutes]
                            s))))

(defn shift [n s]
  (let [dn (if (pos? n)
             (- (count s) n)
             (* -1 n))]
    (apply str (take (count s) (drop dn (cycle s))))))

(defn step [s n f]
  (let [nth-letters (set (take-nth n (drop (dec n) alphabet)))]
    (substitute s nth-letters (f (filter nth-letters s)))))

(defn partition-by-ns [ns coll]
  (first (reduce (fn [[acc coll] n]
                   [(conj acc (take n coll)) (drop n coll)])
                 [[] coll]
                 ns)))

(defn count-step [s]
  (let [lens (reverse (map count (str/split s #" ")))
        s (str/replace s #" " "")]
    (->> (partition-by-ns lens s)
         (map (partial apply str))
         (str/join " "))))

(defn scramble [s]
  (-> s
      (step 2 reverse)
      (step 3 (partial shift 1))
      (step 4 (partial shift -1))
      (count-step)))

(defn unscramble [s]
  (-> s
      (count-step)
      (step 4 (partial shift 1))
      (step 3 (partial shift -1))
      (step 2 reverse)))


(defn -main [& args]
  (println (unscramble (read-line))))
