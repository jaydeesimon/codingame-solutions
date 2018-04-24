(ns simple-cpu-emulation.core
  (:gen-class))

(defn parse-int [radix s]
  (Integer/parseInt s radix))

(defmulti execute* (fn [_ instruction]
                     (first instruction)))

(defmethod execute* \0 [registers _]
  (do
    (println (get registers \0 0)
             (get registers \1 0)
             (get registers \2 0))
    [registers :exit]))

(defn nn [instruction]
  (->> (take-last 2 instruction)
       (apply str)
       (parse-int 16)))

(defmethod execute* \1 [registers instruction]
  [(assoc registers (second instruction) (nn instruction)) :continue])

(defmethod execute* \2 [registers instruction]
  (let [[_ _ x y] instruction
        vx (get registers x 0)
        vy (get registers y 0)
        registers (assoc registers x (bit-and (+ vx vy) 0xFF))]
    [(if (> (+ vx vy) 0xFF)
       (assoc registers \2 1)
       (assoc registers \2 0))
     :continue]))

(defmethod execute* \3 [registers instruction]
  (let [[_ _ x y] instruction
        vx (get registers x 0)
        vy (get registers y 0)
        registers (assoc registers x (bit-and (- vx vy) 0xFF))]
    [(if (< (- vx vy) (get registers y 0))
       (assoc registers \2 1)
       (assoc registers \2 0))
     :continue]))

(defmethod execute* \4 [registers instruction]
  (let [[_ _ x y] instruction
        vx (get registers x 0)
        vy (get registers y 0)]
    [(assoc registers x (bit-or vx vy)) :continue]))

(defmethod execute* \5 [registers instruction]
  (let [[_ _ x y] instruction
        vx (get registers x 0)
        vy (get registers y 0)]
    [(assoc registers x (bit-and vx vy)) :continue]))

(defmethod execute* \6 [registers instruction]
  (let [[_ _ x y] instruction
        vx (get registers x 0)
        vy (get registers y 0)]
    [(assoc registers x (bit-xor vx vy)) :continue]))

(defmethod execute* \7 [registers instruction]
  (if (= (nn instruction) (get registers (second instruction) 0))
    [registers :skip]
    [registers :continue]))

(defmethod execute* \8 [registers instruction]
  (if (not= (nn instruction) (get registers (second instruction) 0))
    [registers :skip]
    [registers :continue]))

(defmethod execute* \9 [registers instruction]
  (let [[_ _ x y] instruction]
    (if (= (get registers x 0)
           (get registers y 0))
      [registers :skip]
      [registers :continue])))

(defmethod execute* \A [registers instruction]
  (let [[_ _ x y] instruction]
    (if (not= (get registers x 0)
              (get registers y 0))
      [registers :skip]
      [registers :continue])))


(defn execute [program]
  (reduce (fn [[registers status] instruction]
            (condp = status
              :exit (reduced registers)
              :skip [registers :continue]
              (execute* registers instruction)))
          [{} :continue]
          (partition 4 program)))

(defn -main [& _]
  (execute (read-line)))

(comment

  (let [d "1005110520010000"]
    (execute d))

  (let [d "100B1106300111022001110840010000"]
    (execute d))

  (let [d "109111C0200120020000"]
    (execute d))

  (let [d "100F111271122001A0010000300100002001"]
    (execute d))

  (partition 4 "100F111271122001A0010000300100002001")

  )


