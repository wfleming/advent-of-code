#!/usr/bin/env racket
#lang racket

(require math/base)
; raco pkg install collections-lib
;(require data/collection)

(define (read-freqs path)
  ; hit a sort of odd quirk with streams:
  ; sequence->stream seems to be lazy or eager, depending on the
  ; the underlying type of sequence. When applied to the result of
  ; sequence-map over in-lines, it was eager, and so entered an infinite loop
  ; in (sequence->stream (in-cycle ...)).
  ; But it seems to be fine if the input to in-cycle is a list that has been
  ; converted to a sequence, so I take it the underlying "type" of sequence
  ; matters. Working with lists for, e.g., sum, seemed easier, so I do the
  ; conversion. I briefly considered using data/collection from
  ; collections-lib for a more generic interface to all collections,
  ; but that seemed to change the semantics of some other existing defs, so
  ; I'm leaving it alone for now.
  (sequence->list
    (sequence-map
      (lambda (line)
        (string->number line))
      (in-lines (open-input-file path)))))

; return the first frequency seen twice, given changes
; to base frequency of 0
(define (find-repeat deltas)
  (find-repeat*
    #(0)
    (sequence->stream (in-cycle deltas))))

(define (vector-last vec)
  (vector-ref vec (- (vector-length vec) 1)))

; there has got to be a more idiomatic way of doing this
(define (vector-push vec val)
  (vector-append
    vec
    (list->vector (cons val '()))))

; given list of frequencies seen & cycling list of deltas,
; return next freq if already seen, otherwise recurse
; seen-freqs is a list, deltas is an infinite stream
(define (find-repeat* seen-freqs deltas)
  (let*
    ([next-freq (+ (vector-last seen-freqs) (stream-first deltas))])
    ; (printf "DEBUG find-repeat* seen=~a next-freq=~a\n" seen-freqs next-freq)
    (if (vector-member next-freq seen-freqs)
      next-freq
      (find-repeat*
        (vector-push seen-freqs next-freq)
        (stream-rest deltas)))))

(let*
  ( [input-file-path (vector-ref (current-command-line-arguments) 0)]
    [ns (read-freqs input-file-path)]
    [total (sum ns)]
    [first-repeat (find-repeat ns)])
  (printf "Going to read from ~a\n" input-file-path)
  (printf "p1: sum is ~a\n" total)
  (printf "p2: first repeated is ~a\n" first-repeat))
