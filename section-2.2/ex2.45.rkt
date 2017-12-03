#lang sicp

(define (split first-op second-op)
  (define (split-op painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-op painter (- n 1))))
          (first-op painter (second-op smaller smaller)))))
  split-op)

(define right-split (split beside below))
(define up-split (split below beside))
