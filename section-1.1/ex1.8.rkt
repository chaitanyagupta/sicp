#lang sicp

;; Ex 1.8

(define (cubert-iter guess previous x)
  (if (good-enough? guess previous)
      guess
      (cubert-iter (improve guess x)
                   guess
                   x)))

(define (improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess))
     3))

(define (square x)
  (* x x))

(define (good-enough? guess previous)
  (< (/ (abs (- previous guess))
        previous)
     0.001))

(define (cubert x)
  (cubert-iter (improve 1.0 x) 1.0 x))
