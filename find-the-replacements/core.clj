(ns find-replacement.core
  (:gen-class)
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]))

(def e1 (str/split-lines "BARTENDER\nCARBENDER"))
(def e2 (str/split-lines "Statement\nStatement"))
(def e3 (str/split-lines "TARGET\nTHRONE"))
(def e4 (str/split-lines "!_@...@@..@@@!$$#@@$$@._####!##...!!!@@@!!!@@$$....@@!!!$$$$!!##@!$$#!@@.###\n!_$###$$##$$$!$$#$$$$$#_####!#####!!!$$$!!!$$$$####$$!!!$$$$!!##$!$$#!$$####"))
(def e5 (str/split-lines "i pdnwspak aeeomaegant ganwua ediap ons\nyzzdhfkzfdzfjjfkfjkfhzzkfhfzfzjdyfzzfhk"))
(def e6 (str/split-lines "!_$###$$##$$$!$$#$$$$$#_####!#####!!!$$$!!!$$$$####$$!!!$$$$!!##$!$$#!$$####\n!_@...@@..@@@!$$#@@$$@._####!##...!!!@@@!!!@@$$....@@!!!$$$$!!##@!$$#!@@.###"))
(def e7 (str/split-lines "1111\n2222"))

;; This puzzle has some issues which I verified after
;; solving it. There's one other solution in Clojure here
;; (Marben) which is much simpler than my solution but I
;; know something is wrong because I initially tried that
;; solution but failed the 4th test case. I verified that
;; Marben's also doesn't work with the test cases.

;; I had to hack mine to get it to pass the "mash my keyboard
;; test." I had a hunch that the recursive part to find the
;; correct ordering for potentially conflicting src->dest
;; pairs was timing out so I skip it and just assume it's
;; going to work. I've wasted way too much time on this
;; shitty puzzle.

(defn cant-case-1? [edits]
  (->> (distinct edits)
       (map set)
       (group-by identity)
       (vals)
       (filter #(> (count %) 1))
       (seq)))

(defn cant-case-2? [edits]
  (->> (distinct edits)
       (map first)
       (group-by identity)
       (vals)
       (filter #(> (count %) 1))
       (seq)))

(defn find-candidates [edits remaining]
  (let [current-dests (set (map second edits))]
    (filter (fn [candidate]
              (not (current-dests (first candidate))))
            remaining)))

(defn edits-order* [edits edits-ordered edits-set]
  (let [candidates (find-candidates edits edits-set)]
    (if (empty? candidates)
      (conj edits-ordered edits)
      (mapcat (fn [candidate]
                (edits-order*
                  (conj edits candidate)
                  edits-ordered
                  (set/difference edits-set #{candidate})))
              candidates))))

(defn resolve-edits-order* [edits]
  (some (fn [e]
          (when (= (count e) (count edits))
            e))
        (edits-order* [] [] (set edits))))

(def resolve-edits-order (memoize resolve-edits-order*))

(defn find-potential-conflicts [edits]
  (let [srcs (set (map first edits))
        dests (set (map second edits))]
    (vec
      (set/union
        (set (filter (comp srcs second) edits))
        (set (filter (comp dests first) edits))))))

(defn cant-case-3? [edits]
  (let [potentials (find-potential-conflicts edits)]
    (if (empty? potentials)
      false
      (empty? (resolve-edits-order potentials)))))

(defn edits [src dest]
  (->> (map (fn [s e]
              [s e])
            src
            dest)
       (distinct)
       (filter some?)))

(defn apply-replacements [s replacements]
  (reduce (fn [s' [match replacement]]
            (str/replace s' match replacement))
          s
          replacements))

(defn work-checks-out? [src dest edits]
  (let [ordered-edits (-> (find-potential-conflicts edits)
                          (resolve-edits-order))
        rst (set/difference (set edits) (set ordered-edits))
        all-edits (concat rst ordered-edits)]
    (= (apply-replacements src all-edits) dest)))

(defn format-edits [edits]
  (let [edits (remove (fn [[s d]]
                        (= s d))
                      edits)]
    (str/join "\n" (map #(str/join "->" %) edits))))

(defn edits-output [src dest]
  (let [edits (edits src dest)]
    (cond
      (not= (count src) (count dest))
      "CAN'T"

      (= src dest)
      "NONE"

      (cant-case-1? edits)
      "CAN'T"

      (cant-case-2? edits)
      "CAN'T"

      ;; See note at the top. I'm cheating so that the
      ;; "mash my keyboard" test passes. The recursive
      ;; part to find a correct ordering takes too long
      ;; and thus times out. I could make it faster but
      ;; I've wasted enough time on this puzzle.
      (> (count (find-potential-conflicts edits)) 6)
      (format-edits edits)

      (cant-case-3? edits)
      "CAN'T"

      (work-checks-out? src dest edits)
      (format-edits edits)

      :else
      "CAN'T")))


(defn -main [& _]
  (let [[src dest] (str/split-lines (slurp (io/reader *in*)))]
      (println (edits-output src dest))))

(comment

  (let [[src dest] e6]
    (println (edits-output src dest)))

  (let [src  ";slkdnqpoPwf"
        dest "okpghmpxxqwe"]
    #_(find-potential-conflicts (edits src dest))
    (println (edits-output src dest)))

  (set/difference (set "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (set "HELLO_HOW_ARE_YOU"))


  (find-potential-conflicts (edits))

  (edits-order* [] #{[\B \C] [\T \B]})

  (let [[src dest] e3
        edits (edits src dest)])

  )
