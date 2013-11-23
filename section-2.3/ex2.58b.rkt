#lang planet neil/sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (before item lst)
  (define (iter lst result)
    (cond 
      ((null? lst) result)
      ((eq? (car lst) item) result)
      (else (iter (cdr lst) (cons (car lst) result)))))
  (reverse (iter lst nil)))

(define (after item lst)
  (cdr (memq item lst)))

(define (first-term operator)
  (lambda (x)
    (let ((items (before operator x)))
      (if (= (length items) 1)
          (car items)
          items))))

(define (second-term operator)
  (lambda (x)
    (let ((items (after operator x)))
      (if (= (length items) 1)
          (car items)
          items))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
   (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (sum? x)
  (and (pair? x) (memq '+ x)))

(define addend (first-term '+))
(define augend (second-term '+))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (product? x)
  (and (pair? x) (not (sum? x)) (memq '* x)))

(define multiplier (first-term '*))
(define multiplicand (second-term '*))