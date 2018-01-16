#lang sicp

;;; copy the following forms in the REPL for ambiguous evaluator

(define (not x)
  (if x false true))

(define (require p)
  (if (not p) (amb)))

(define (member item items)
  (cond ((null? items) false)
        ((eq? item (car items)) items)
        (else (member item (cdr items)))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (xor a b)
  (cond ((and a (not b)) true)
        ((and (not a) b) true)
        (else false)))

(define (liars)
  (let ((betty (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5)))
    (require (xor (= kitty 2) (= betty 3)))
    (let ((mary (amb 1 2 3 4 5)))
      (require (xor (= kitty 2) (= mary 4)))
      (require (xor (= mary 4) (= betty 1)))
      (let ((ethel (amb 1 2 3 4 5))
            (joan (amb 1 2 3 4 5)))
        (require (xor (= ethel 1) (= joan 2)))
        (require (xor (= joan 3) (= ethel 5)))
        (require (distinct? (list betty kitty mary ethel joan)))
        (list (cons 'betty betty)
              (cons 'kitty kitty)
              (cons 'mary mary)
              (cons 'ethel ethel)
              (cons 'joan joan))))))