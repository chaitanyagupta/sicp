#lang sicp

(define (parity x)
  (remainder x 2))

(define (same-parity first . rest)
  (let ((p1 (parity first)))
    (define (same-parity-list x)
      (if (null? x)
          nil
          (if (= (parity (car x)) p1)
              (cons (car x) (same-parity-list (cdr x)))
              (same-parity-list (cdr x)))))
    (cons first (same-parity-list rest))))

