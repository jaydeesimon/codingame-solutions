(ns Player
  (:gen-class))

(defn -main [& args]
  (let [lightX (read) lightY (read) initialTX (read) initialTY (read)]
    (loop [x initialTX y initialTY]
      (let [remainingTurns (read)
            dx (compare lightX x)
            dy (compare lightY y)]

        (println (str (case dy -1 "N" 0 "" 1 "S")
                      (case dx -1 "W" 0 "" 1 "E")))
        (recur (+ x dx) (+ y dy))))))
