#lang sicp

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (define (iter base exponent a)
    (cond ((= exponent 0) a)
          ((even? exponent) (iter (square base) (/ exponent 2) a))
          (else (iter base (- exponent 1) (* base a)))))
  (iter b n 1))
