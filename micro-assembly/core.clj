(ns microassembly.core
  (:gen-class)
  (:require [clojure.string :as str]))

(defn- resolve-src [registers src]
  (or (get registers src) (Integer/parseInt src)))

(defmulti interpret (fn [[line _ _]]
                      (first line)))

(defn- basic-op [[_ dest & srcs] line-num registers f]
  (let [dest-val (apply f (map (partial resolve-src registers) srcs))]
    [(inc line-num) (assoc registers dest dest-val)]))

(defmethod interpret "MOV" [[line line-num registers]]
  (basic-op line line-num registers identity))

(defmethod interpret "ADD" [[line line-num registers]]
  (basic-op line line-num registers +))

(defmethod interpret "SUB" [[line line-num registers]]
  (basic-op line line-num registers -))

(defmethod interpret "JNE" [[line line-num registers]]
  (let [[_ jump-line-num register src] line
        jump-line-num (Integer/parseInt jump-line-num)
        register-val (resolve-src registers register)
        src (resolve-src registers src)]
    (if (not= register-val src)
      [jump-line-num registers]
      [(inc line-num) registers])))

(defn execute* [registers lines-tokenized]
  (let [lines-map (zipmap (range (count lines-tokenized)) lines-tokenized)]
    (loop [registers registers
           next-line-num 0]
      (let [line (get lines-map next-line-num)]
        (if (nil? line)
          registers
          (let [[next-instruction registers] (interpret [line next-line-num registers])]
            (recur registers next-instruction)))))))

(defn execute [initial-registers lines]
  (let [lines-tokenized (map #(re-seq #"\S+" %) lines)]
    (execute* initial-registers lines-tokenized)))

(defn -main [& _]
  (let [registers {"a" (read) "b" (read) "c" (read) "d" (read)}
        _ (read-line)
        _ (read-line)
        source (slurp *in*)
        registers (execute registers (str/split-lines source))]
    (println (get registers "a")
             (get registers "b")
             (get registers "c")
             (get registers "d"))))
