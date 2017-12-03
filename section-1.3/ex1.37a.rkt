#lang sicp

(define (cont-frac n d k)
  (define (recur i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(define (get-phi k)
  (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k)))
