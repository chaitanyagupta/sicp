#lang sicp

;;; copy the following forms in the REPL for ambiguous evaluator

(define (not x)
  (if x false true))

(define (require p)
  (if (not p) (amb)))

(define (square x)
  (* x x))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between a b)
  (require (< a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (a-pythagorean-triple low)
  (let* ((j (an-integer-starting-from low))
         (i (an-integer-between low j))
         (k (sqrt (+ (square i) (square j)))))
    (require (integer? k))
    (list i j k)))
