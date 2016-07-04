(ns triforce.core)

(defn pyramid [n ch]
  (->> (repeat ch)
       (map repeat (filter odd? (range)))
       (take n)
       (map (partial apply str))))

(defn center [width s]
  (let [sn (quot (count s) 2)
        wn (quot width 2)
        padding (apply str (repeat (- wn sn) \space))]
    (str padding s)))

(defn triforce [n]
  (let [top (pyramid n \*)
        bottom-left (pyramid n \*)
        bottom-center (reverse (pyramid n \space))
        bottom-right (pyramid n \*)
        bottom (map str bottom-left bottom-center bottom-right)
        width (count (last bottom))]
    (map (partial center width) (concat top bottom))))

(defn dot [triforce]
  (concat (list (apply str (concat [\.] (rest (first triforce)))))
          (rest triforce)))

(defn -main [& args]
  (let [n (read)]
    (doseq [line (dot (triforce n))]
      (println line))))
