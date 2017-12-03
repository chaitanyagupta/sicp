#lang sicp

(define dx 0.00000001)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

(define smooth
  (lambda (f)
    (lambda (x) 
      (/ (+ (f x)
            (f (+ x dx))
            (f (- x dx)))
         3))))

(define n-smooth
  (lambda (f n)
    ((repeated smooth n) f)))

(define (square x)
  (* x x))

((n-smooth square 3) 2)

