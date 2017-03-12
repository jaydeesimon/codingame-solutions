(ns xorandor.util
  (:require [clojure.java.io :as io]))

(defn case-input [n]
  (-> (io/resource (str "case0" n ".txt"))
      (slurp)))

