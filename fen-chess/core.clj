(ns chess-moves-fen-position.core
  (:require [clojure.pprint :as pp])
  (:gen-class))

(defn normalize-chess-coord [[col rank]]
  (let [cols "abcdefgh"]
    [(- (count cols) (read-string (str rank)))
     (get (zipmap cols (range (count cols))) col)]))

(defn parse-move [s]
  (let [[start target] (map normalize-chess-coord (partition 2 s))
        move {:start start :target target}]
    (if (= (count s) 5)
      (assoc move :promotion (-> s last str keyword))
      move)))

(defn fen->board [fen]
  (->> (partition-by (partial = \/) fen)
       (remove #(= (first %) \/))
       (map #(mapcat (fn [c]
                       (if (integer? (read-string (str c)))
                         (repeat (read-string (str c)) \.)
                         [c])) %))
       (map #(map (comp keyword str) %))
       (mapv vec)))

(defn board->fen [board]
  (->> (interpose \/ board)
       (flatten)
       (partition-by (partial = :.))
       (map #(if (= (first %) :.)
              (str (count %))
              %))
       (flatten)
       (map #(if (keyword? %) (name %) %))
       (clojure.string/join)))

(defn piece [board coord]
  (get-in board coord))

(defn piece-type [board coord]
  (keyword (clojure.string/lower-case (name (get-in board coord)))))

(defn distance [[x1 y1] [x2 y2]]
  (Math/sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(defn castling? [board {:keys [start target]}]
  (and (= (piece-type board start) :k)
       (> (distance start target) 1)
       (#{[0 4] [7 4]} start)))

(defn en-passant?
  [board {prev-start :start prev-target :target :as prev-move} {start :start target :target}]
  (if (nil? prev-move)
    false
    (let [passant-inited? (and (= (piece-type board prev-target) :p)
                               (== (distance prev-start prev-target) 2))
          passant-completed? (and (= (piece-type board start) :p)
                                  (= target (mapv + prev-target (if (= (piece board prev-target) :P)
                                                                  [1 0]
                                                                  [-1 0]))))]
      (and passant-inited? passant-completed?))))

(defn move-type [board prev-move move]
  (cond (get move :promotion) :pawn-promotion
        (castling? board move) :castling
        (en-passant? board prev-move move) :en-passant
        :else :regular))

(defn castling-move [{:keys [start target]}]
  (cond (and (= start [0 4]) (= target [0 6])) [[[0 4] :.] [[0 6] :k] [[0 7] :.] [[0 5] :r]]
        (and (= start [0 4]) (= target [0 2])) [[[0 4] :.] [[0 2] :k] [[0 0] :.] [[0 3] :r]]
        (and (= start [7 4]) (= target [7 6])) [[[7 4] :.] [[7 6] :K] [[7 7] :.] [[7 5] :R]]
        (and (= start [7 4]) (= target [7 2])) [[[7 4] :.] [[7 2] :K] [[7 0] :.] [[7 3] :R]]
        :else (throw (ex-info (format "Illegal castling move: %s -> %s" start target) {}))))

(defn pawn-promotion-move [{:keys [start target promotion]}]
  [[start :.] [target promotion]])

(defn en-passant-move
  [board {prev-target :target} {start :start target :target}]
  [[prev-target :.] [start :.] [target (piece board start)]])

(defn regular-move [board {:keys [start target]}]
  [[start :.] [target (piece board start)]])

(defn execute-move-steps [board move-steps]
  (reduce (fn [board [coord piece]]
            (assoc-in board coord piece))
          board
          move-steps))

(defn execute-moves [board moves]
  (reduce (fn [board [prev-move move]]
            (let [move-steps (case (move-type board prev-move move)
                               :pawn-promotion (pawn-promotion-move move)
                               :castling (castling-move move)
                               :en-passant (en-passant-move board prev-move move)
                               :regular (regular-move board move))]
              (execute-move-steps board move-steps)))
          board
          (partition 2 1 (concat [nil] moves))))

(defn -main [& _]
  (let [board (fen->board (read-line))
        n (read)
        _ (read-line)
        moves (map parse-move (repeatedly n read-line))]
    (println (board->fen (execute-moves board moves)))))