#lang sicp

(define (for-each proc list)
  (if (null? list)
      #t
      ((lambda ()
         (proc (car list))
         (for-each proc (cdr list))))))
