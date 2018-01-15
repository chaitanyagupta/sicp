#lang sicp

(define (square x)
  (* x x))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (a-pythagorean-triple low)
  (let* ((j (an-integer-starting-from low))
         (i (an-integer-between low j))
         (k (sqrt (+ (square i) (square j)))))
    (require (integer? k))
    (list i j k)))
