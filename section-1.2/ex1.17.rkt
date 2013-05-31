#lang planet neil/sicp

(define (double x)
  (+ x x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= b 0) 0)
        ((even? b) (mul (double a) (halve b)))
        (else (+ a (mul a (- b 1))))))
