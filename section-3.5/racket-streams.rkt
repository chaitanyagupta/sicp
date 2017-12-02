#lang racket

(define (average x y) (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (newline)
  (display "SQRT-IMPROVE: ")
  (display guess)
  (display " ")
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (stream-cons 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream x))))
