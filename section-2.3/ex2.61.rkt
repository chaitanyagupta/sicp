#lang sicp

(define (adjoin-set x set)
  (if (null? set) 
      (list x)
      (let ((first (car set)))
        (cond ((= x first) set)
              ((< x first) (cons x set))
              (else (cons first (adjoin-set x (cdr set))))))))
