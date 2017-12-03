#lang sicp

;; Ex 1.7
;; Failure examples: (sqrt 1e100), (sqrt 4e-12)

(define (sqrt-iter guess previous x)
  (if (good-enough? guess previous)
      guess
      (sqrt-iter (improve guess x)
                 guess
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess previous)
  (< (/ (abs (- previous guess))
        previous)
     0.001))

(define (sqrt x)
  (sqrt-iter (improve 1.0 x) 1.0 x))

