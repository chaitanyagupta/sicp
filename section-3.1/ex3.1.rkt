#lang planet neil/sicp

(define (make-accumulator n)
  (lambda (x)
    (set! n (+ n x))
    n))

