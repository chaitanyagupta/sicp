#lang sicp

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (inc n) (+ n 1))

(define (get-pi n)
  (define (term a)
    (if (= (remainder a 2) 0)
        (/ a (+ a 1))
        (/ (+ a 1) a)))
  (* 4.0 (product term 2 inc n)))
