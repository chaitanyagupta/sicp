#lang sicp

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; Ex 1.2
(/ (+ 5
      4
      (- 2
         3
         (+ 6 1/5)))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Ex 1.3

(define (larger x y)
  (if (> x y) x y))

(define (sum-of-larger-two-squares x y z)
  (cond ((> x y) (sum-of-squares x (larger y z)))
        ((> y z) (sum-of-squares y (larger z x)))
        (else (sum-of-squares z (larger x y)))))

;; Ex 1.4
;; a plus abs(b)

;; Ex 1.5
;; In applicative order, program would recurse infinitely
;; In normal order, 0 would be returned

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;;; Section 1.1.7

;; Capture original sqrt

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; Ex 1.6
;; sqrt-iter recurses infinitely
