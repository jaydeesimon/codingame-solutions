(ns chuck-norris.core
  (:require [clojure.string :refer [join]])
  (:gen-class))

;; Chuck Norris
;; https://www.codingame.com/ide/4384946526b021e9aa84fa08a67b85ba5d813b5

(defn pad-with-zeroes [binary]
  (let [num (- 7 (count binary))]
    (str (join (repeat num "0")) binary)))

(defn int->binary [n]
  (pad-with-zeroes (Integer/toBinaryString n)))

(defn str->binary [s]
  (join (map int->binary (map int s))))

(defn binary->zero-encoded [binary]
  (->> (partition-by identity binary)
       (map (fn [[frst :as xs]]
              (let [zero-encoded (join (repeat (count xs) "0"))]
                (if (= frst \1)
                  (str "0 " zero-encoded)
                  (str "00 " zero-encoded)))))
       (interpose " ")
       (join)))

(defn -main [& args]
  (let [message (read-line)]
    (-> message
        (str->binary)
        (binary->zero-encoded)
        (println))))
