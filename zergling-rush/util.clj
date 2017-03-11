(ns zergling-rush.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn case->board [n]
  (let [filename (str "case" n ".txt")]
    (->> (slurp (io/resource filename))
         (str/split-lines)
         (mapv vec))))
