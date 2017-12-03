#lang sicp

(define (accumulate combiner null-value term a next b)
  (define (recur a)
    (if (> a b)
        null-value
        (combiner (term a) 
                  (recur (next a)))))
  (recur a))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

(define (identity x)
  x)

(define (sum-cubes a b)
  (sum cube a inc b))

(define (get-pi n)
  (define (term a)
    (if (= (remainder a 2) 0)
        (/ a (+ a 1))
        (/ (+ a 1) a)))
  (* 4.0 (product term 2 inc n)))
