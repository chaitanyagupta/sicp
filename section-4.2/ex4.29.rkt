#lang sicp

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define (square x)
  (* x x))

;;;; without memoization
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2

;;;; with memoization
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1
