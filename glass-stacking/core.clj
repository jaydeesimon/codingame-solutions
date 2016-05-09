(ns glasses.core
  (:require [clojure.string :refer [join]])
  (:gen-class))

;; ASCII ART : Glass Stacking
;; https://www.codingame.com/ide/4360250b8147e8b812efd6e79e217dff9d37d4e

(defn pyramid-seq
  "Returns a lazy sequence of vectors where vector is
  [<sum of glasses in pyramid> <rows in pyramid>]"
  []
  (iterate (fn [[acc i]]
             [(+ (inc i) acc) (inc i)])
           [1 1]))

(defn min-pyramid-rows
  "Given n glasses, returns the min number
  of rows to make a pyramid."
  [n]
  (some (fn [[[n-min row-cur] [n-max _]]]
          (when (and (>= n n-min) (< n n-max))
            row-cur))
        (partition 2 1 (pyramid-seq))))

(defn center
  "Returns a string, s, centered in width, padded by spaces."
  [s width]
  (let [left-pad (Math/floor (/ (- width (count s)) 2))
        right-pad (- width (count s) left-pad)]
    (str (join (repeat left-pad " ")) s (join (repeat right-pad " ")))))

(defn glasses-row
  "Returns a sequence of strings that represents
  a row of glasses centered in width."
  [n width]
  (let [tops (join (interpose "   " (repeat n "***")))
        upper-mids (join (interpose "   " (repeat n "* *")))
        lower-mids (join (interpose "   " (repeat n "* *")))
        bottoms (join (interpose " " (repeat n "*****")))]
    [(center tops width)
     (center upper-mids width)
     (center lower-mids width)
     (center bottoms width)]))

(defn pyramid-str
  "Given the amount of rows, returns a sequence of strings
  that represent an entire pyramid."
  [rows]
  ;; the width is the bottom of the pyramid. 5 is the width
  ;; of one glass and (dec rows) is added for the spaces in
  ;; between
  (let [width (+ (* 5 rows) (dec rows))]
    (mapcat #(glasses-row % width) (range 1 (inc rows)))))

(defn -main [& args]
  (let [n (read)]
    (doall (map println (pyramid-str (min-pyramid-rows n))))))
