#lang sicp

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (inc n) (+ n 1))

(define (get-pi n)
  (define (term a)
    (if (= (remainder a 2) 0)
        (/ a (+ a 1))
        (/ (+ a 1) a)))
  (* 4.0 (product term 2 inc n)))
