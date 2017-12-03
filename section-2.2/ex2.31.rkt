#lang sicp

(define (square x) (* x x))

(define (tree-map fn tree)
  (cond ((null? tree) nil)
        ((pair? tree) (cons (tree-map fn (car tree))
                            (tree-map fn (cdr tree))))
        (else (fn tree))))
