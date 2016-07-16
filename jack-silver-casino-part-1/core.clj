(ns jack-silver-casino-part-1.core
  (:gen-class))

(defn bet [amount]
  (int (Math/ceil (* 0.25 amount))))

(defmulti amount-after-round (fn [_ round]
                               (keyword (second round))))

(defn amount-after-round* [amount win? bet odds ball]
  (if (and win? (not (zero? ball)))
    (+ amount (* bet odds))
    (- amount bet)))

(defmethod amount-after-round :ODD [amount [ball _]]
  (amount-after-round* amount (odd? ball) (bet amount) 1 ball))

(defmethod amount-after-round :EVEN [amount [ball _]]
  (amount-after-round* amount (even? ball) (bet amount) 1 ball))

(defmethod amount-after-round :PLAIN [amount [ball _ number]]
  (amount-after-round* amount (= ball number) (bet amount) 35 ball))

(defn -main [& args]
  (let [rounds (read) cash (read) _ (read-line)]
    (->> (repeatedly rounds read-line)
         (map #(clojure.string/split % #"\s"))
         (map (partial mapv read-string))
         (reduce amount-after-round cash)
         (println))))
