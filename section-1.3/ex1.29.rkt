#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (inc x)
  (+ 1 x))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) 
    (f (+ a (* k h))))
  (define (term x)
    (cond 
      ((or (= x 0) (= x n)) (y x))
      ((= (remainder x 2) 0) (* 2 (y x)))
      ((= (remainder x 2) 1) (* 4 (y x)))))
  (* (/ h 3) (sum term 0 inc n)))
