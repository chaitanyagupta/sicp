#lang planet neil/sicp

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; compute the fixed point using (fixed-point (lambda (x) (/ (log 1000) (log x))) 10.00)
;; Avoid using 1.0 as the first guess
