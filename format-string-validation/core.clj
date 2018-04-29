(ns format-string-validation.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn ->puzzle-regex [s]
  (str/join
    (mapcat (fn [c]
              (condp = c
                \~ ["." "*"]
                \? ["."]
                ["\\Q" (str c) "\\E"]))
            s)))

(defn -main [& _]
  (let [word (read-line)
        pattern (read-line)]
    (if (re-matches (re-pattern (->puzzle-regex pattern)) word)
      (println "MATCH")
      (println "FAIL"))))
