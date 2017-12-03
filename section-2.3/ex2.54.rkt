#lang sicp

(define (equal? a b)
  (cond 
    ((null? a) (null? b))
    ((pair? a)
     (and (pair? b)
          (equal? (car a) (car b))
          (equal? (cdr a) (cdr b))))
    ((not (pair? a)) (and (not (pair? b)) (eq? a b)))
    (else false)))
    
