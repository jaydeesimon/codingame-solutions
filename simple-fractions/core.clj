(ns simple-fractions.core
  (:gen-class))

(defn mixed-number [numer denom]
  (let [sign (Integer/signum (* numer denom))
        quotient (* sign (quot numer denom))
        fraction (* sign (/ (rem numer denom) denom))]
    [sign quotient fraction]))

(defn format-mixed-number* [[sign int-part fraction :as mixed-number]]
  (cond (every? zero? mixed-number) "0"
        (and (zero? int-part) (not (zero? fraction))) (str (* sign fraction))
        (and (not (zero? int-part)) (zero? fraction)) (str (* sign int-part))
        :else (str (* sign int-part) " " fraction)))

(defn format-mixed-number [numer denom]
  (if (zero? denom)
    "DIVISION BY ZERO"
    (format-mixed-number* (mixed-number numer denom))))

(defn -main [& _]
  (let [n (read) _ (read-line)]
    (->> (repeatedly n read-line)
         (map #(clojure.string/split % #"/"))
         (map (partial mapv read-string))
         (map (partial apply format-mixed-number))
         (clojure.string/join "\n")
         (println))))
