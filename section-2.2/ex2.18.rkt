#lang planet neil/sicp

(define (reverse x)
  (if (null? x)
      nil
      (append (reverse (cdr x)) (list (car x)))))

