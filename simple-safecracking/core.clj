(ns simple-safecracking.core
  (:gen-class))

(def alphabet "abcdefghijklmnopqrstuvwxyz")

(defn offset [ch]
  (count (take-while (partial not= ch) alphabet)))

(defn alphabet-cycle [ch]
  (take (count alphabet) (drop (offset ch) (cycle alphabet))))

(defn cipher [ch-key ch-val]
  (into {} (map vector (alphabet-cycle ch-key) (alphabet-cycle ch-val))))

(def number-words {"zero" 0 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9})

(defn -main [& args]
  (let [[sentence numbers] (clojure.string/split (read-line) #": ")]
    (->> (map (cipher (second sentence) \h) numbers)
         (partition-by nil?)
         (map (partial apply str))
         (filter (comp pos? count))
         (map number-words)
         (apply str)
         (println))))