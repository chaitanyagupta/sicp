#lang sicp

(define (contains-cycle? x)
  (define (pair-encountered? pair encountered-pairs)
    (cond ((null? encountered-pairs) false)
          ((eq? pair (car encountered-pairs)) true)
          (else (pair-encountered? pair (cdr encountered-pairs)))))
  (define (contains-cycle-aux x encountered-pairs)
    (cond ((not (pair? x)) false)
          ((pair-encountered? x encountered-pairs) true)
          (else (let ((encountered-pairs (cons x encountered-pairs)))
                  (or (contains-cycle-aux (car x) encountered-pairs)
                      (contains-cycle-aux (cdr x) encountered-pairs))))))
  (contains-cycle-aux x '()))


