#lang sicp

(define (filtered-accumulate combiner filter null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (identity x)
  x)

(define (square x)
  (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (= (remainder n 2) 1))

(define (inc n) 
  (+ n 1))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (if (= n 1)
      #f
      (= n (smallest-divisor n))))

(define (sum-prime-squares a b)
  (filtered-accumulate + prime? 0 square a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relative-prime-product n)
  (define (relative-prime? a)
    (= (gcd a n) 1))
  (filtered-accumulate * relative-prime? 1 identity 1 inc (- n 1)))
