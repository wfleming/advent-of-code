#!/usr/bin/env clj

(require '[clojure.java.io :as io])

; (println "hello world")
; (println (first *command-line-args*))

(defn read-delta [path]
  (with-open [rdr (io/reader path)]
    (doall (map
             (fn [line] (Integer/parseInt line))
             (line-seq rdr)))))

(defn member [x coll]
  (some (fn [y] (= x y)) coll))

(defn find-repeated [deltas]
  (reduce
    (fn [seen-freqs next-delta]
      (let
        [next-freq (+ (first seen-freqs) next-delta)]
        (if (member next-freq seen-freqs)
           (reduced next-freq)
           (cons next-freq seen-freqs))))
    [0] (cycle deltas)))

(let
  [ deltas (read-delta (first *command-line-args*))
    sum (apply + deltas)
    first-repeated (find-repeated deltas)]
  (printf "p1 total = %s\n" sum)
  (printf "p2 first repeated = %s\n" first-repeated))
