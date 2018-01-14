#lang sicp

(define (sum-upto n)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (- n 1) (+ result n))))
  (iter n 0))

(define (rem n q)
  (define (iter m)
    (if (< m q)
        m
        (iter (- m q))))
  (iter n))

(define (even? n)
  (= 0 (rem n 2)))

(define (odd? n)
  (= 1 (rem n 2)))

(define (alternate-sums n bump)
  (let ((sum 0))
    (while (> n 0)
           (set! sum
                 (cond ((= 0 (rem n bump)) (+ sum (* n bump)))
                       ((even? n) (+ sum n))
                       ((odd? n) (+ sum (- n)))))
           (set! n (- n 1)))
    sum))

(define (even/fiver/elevener? n)
  (or (even? n)
      (= 0 (rem n 5))
      (= 0 (rem n 11))))
