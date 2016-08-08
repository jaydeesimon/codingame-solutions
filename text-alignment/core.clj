(ns text-alignment.core
  (:gen-class))

(defn left [width s] s)

(defn right [width s]
  (format (str "%" width "s") s))

(defn center [width s]
  (let [pad-n (quot (- width (count s)) 2)]
    (apply str (concat (repeat pad-n \space) s))))

(defn justify [width s]
  (let [split (clojure.string/split s #" ")
        wc (count split)
        space-removed (clojure.string/replace s #"\s+" "")
        pad-n (quot (- width (count space-removed)) (dec wc))]
    (->> (interpose (repeat pad-n \space) split)
         (map (partial apply str))
         (clojure.string/join))))

(defn align [type width s]
  (condp = type
    "LEFT" (left width s)
    "RIGHT" (right width s)
    "CENTER" (center width s)
    "JUSTIFY" (justify width s)
    (throw (ex-info (str "Unexpected: " type) {:type type}))))

(defn -main [& _]
  (let [alignment (read-line)
        n (read) _ (read-line)
        lines (repeatedly n read-line)
        width (apply max (map count lines))]
    (doseq [line lines]
      (println (align alignment width line)))))
