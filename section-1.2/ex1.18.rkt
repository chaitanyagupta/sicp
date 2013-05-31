#lang planet neil/sicp

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (define (iter x y product)
    (cond ((= y 0) product)
          ((even? y) (iter (double x) (halve y) product))
          (else (iter x (- y 1) (+ product x)))))
  (iter a b 0))
