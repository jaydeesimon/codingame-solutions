(ns next-license-plate.core
  (:require [clojure.set :refer [map-invert]]
            [clojure.java.io :as io])
  (:gen-class))

(def letter->dec (zipmap "ABCDEFGHIJKLMNOPQRSTUVWXYZ" (range 26)))
(def digit->dec (zipmap "0123456789" (range 10)))
(def ch->dec (merge letter->dec digit->dec))
(def dec->letter (map-invert letter->dec))
(def dec->digit (map-invert digit->dec))

(defn- pow [base exp]
  (reduce * (repeat exp base)))

(defn license->number [license]
  (let [[a b c d e f g] (remove #{\-} license)
        n-including-zeroes (+ (* (ch->dec e) (pow 10 0))
                              (* (ch->dec d) (pow 10 1))
                              (* (ch->dec c) (pow 10 2))
                              (* (ch->dec g) (* (pow 26 0) (pow 10 3)))
                              (* (ch->dec f) (* (pow 26 1) (pow 10 3)))
                              (* (ch->dec b) (* (pow 26 2) (pow 10 3)))
                              (* (ch->dec a) (* (pow 26 3) (pow 10 3))))]
    ;; subtract the 000's away
    (- n-including-zeroes (inc (quot n-including-zeroes 1000)))))

(defn number->license [n]
  (let [n (inc (quot (* 1000 n) 999))  ;; account for the 000's
        [e n] ((juxt mod quot) n 10)
        [d n] ((juxt mod quot) n 10)
        [c n] ((juxt mod quot) n 10)
        [g n] ((juxt mod quot) n 26)
        [f n] ((juxt mod quot) n 26)
        [b n] ((juxt mod quot) n 26)
        a (mod n 26)]
    (format "%c%c-%c%c%c-%c%c"
            (dec->letter a)
            (dec->letter b)
            (dec->digit c)
            (dec->digit d)
            (dec->digit e)
            (dec->letter f)
            (dec->letter g))))

(defn license+n [license n]
  (number->license (+ (license->number license) n)))

(defn -main [& _]
  (let [[license n] (map #(%1 %2) [identity read-string] (line-seq (io/reader *in*)))]
    (println (license+n license n))))