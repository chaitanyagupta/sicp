#lang planet neil/sicp

(define (zerop n)
  (= n 0))

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car x)
  (if (zerop (remainder x 2))
      (+ 1 (car (/ x 2)))
      0))

(define (cdr x)
  (if (zerop (remainder x 3))
      (+ 1 (cdr (/ x 3)))
      0))