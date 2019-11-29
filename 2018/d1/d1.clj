#!/usr/bin/env clj

(require '[clojure.java.io :as io])

; (println "hello world")
; (println (first *command-line-args*))

(defn read-delta [path]
  (with-open [rdr (io/reader path)]
    (doall (map
             (fn [line] (Integer/parseInt line))
             (line-seq rdr)))))

(defn find-repeated
  ([deltas] (find-repeated (hash-set) 0 (cycle deltas)))
  ([seen-freqs cur-freq deltas]
;   (println "DEBUG" "seen=" seen-freqs "cur=" cur-freq)
   (if (contains? seen-freqs cur-freq)
     cur-freq
     (recur
       (conj seen-freqs cur-freq)
       (+ cur-freq (first deltas))
       (rest deltas))
   )))

; (defn find-repeated [deltas]
;   (reduce
;     (fn [seen-and-cur next-delta]
;       (let
;         [seen-freqs (first seen-and-cur)
;          cur-freq (second seen-and-cur)
;          next-freq (+ cur-freq next-delta)]
;         (println "DEBUG" "seen=" seen-freqs "cur=" cur-freq "delta=" next-delta)
;         (if (contains? seen-freqs cur-freq)
;            (reduced cur-freq)
;            (list (conj seen-freqs cur-freq) next-freq))))
;     '((hash-set) 0) (cycle deltas)))

(let
  [ deltas (read-delta (first *command-line-args*))
    sum (apply + deltas)
    first-repeated (find-repeated deltas)]
  (printf "p1 total = %s\n" sum)
  (printf "p2 first repeated = %s\n" first-repeated))
