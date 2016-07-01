(ns snake-encoding.core
  (:require [clojure.string :as str])
  (:gen-class))

;; This is not my solution. This is leetwinski's solution.
;; Although, we took the same approach, this is WAY more
;; elegant and easier to follow.

(defn -main [& args]
  (let [size (read)
        i (read)
        _ (read-line)
        n (* size size)]
    (->> (repeatedly size read-line)
         (apply map vector)
         (mapcat #(%1 %2) (cycle [reverse identity]))
         cycle
         (drop (- n i))
         (take n)
         (partition size)
         (map #(%1 %2) (cycle [reverse identity]))
         (apply map str)
         (str/join \newline)
         println)))
