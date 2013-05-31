#lang planet neil/sicp

(define (iterative-improvement good-enough? improve)
  (lambda (guess)
    (if (good-enough? guess)
        guess
        ((iterative-improvement good-enough? improve) (improve guess)))))

(define (iterative-improvment good-enough? improve)
  (lambda (guess)
    ((lambda (f guess)
       (if (good-enough? guess)
           guess
           (f (improve guess))))
     (lambda (f guess)
       (if (good-enough? guess)
           guess
           (f (improve guess))))
     guess)))
     