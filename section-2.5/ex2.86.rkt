#lang planet neil/sicp

;;; Type system

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (attach-tag type-tag contents)
  (if (memq type-tag (list 'integer 'real))
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((and (number? datum)(inexact? datum)) 'real)
        ((integer? datum) 'integer)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((and (number? datum) (inexact? datum)) datum)
        ((integer? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

;;; Generic arithmetic operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arc-tangent x y) (apply-generic 'arc-tangent x y))
(define (square x) (apply-generic 'square x))
(define (square-root x) (apply-generic 'square-root x))

(define (raise x) ((get 'raise (list (type-tag x))) (contents x)))
(define (project x) ((get 'project (list (type-tag x))) (contents x)))

;;; Generic scheme numbers

(define (install-scheme-number-package type)
  (define (tag x)
    (attach-tag type x))    
  (put 'add (list type type)
       (lambda (x y) (tag (+ x y))))
  (put 'sub (list type type)
       (lambda (x y) (tag (- x y))))
  (put 'mul (list type type)
       (lambda (x y) (tag (* x y))))
  (put 'div (list type type)
       (lambda (x y) (tag (/ x y))))
  (put 'sine (list type) sin)
  (put 'cosine (list type) cos)
  (put 'arc-tangent (list type type) 
       (lambda (x y)
         (tag (atan x y))))
  (put 'square (list type) 
       (lambda (x) (tag (* x x))))
  (put 'square-root (list type) sqrt)
  (put 'equ? (list type type)
       (lambda (x y) (= x y)))
  (put '=zero? (list type) zero?)
  'done)

;;; Integers

(define (install-integer-package)
  (define type 'integer)
  (install-scheme-number-package type)
  (define (tag x)
    (attach-tag type x))
  (put 'raise (list type)
       (lambda (x) (make-rational x 1)))
  (put 'make 'integer tag)
  'done)

(define (make-integer x)
  ((get 'make 'integer) x))

(install-integer-package)

;;; Reals

(define (install-real-package)
  (define type 'real)
  (install-scheme-number-package type)
  (define (tag x)
    (attach-tag type x))
  (put 'raise (list type)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project (list type)
       (lambda (x) 
         (let ((exact (inexact->exact x)))
           (make-rational (numerator exact) (denominator exact)))))
  (put 'make 'real tag)
  'done)

(define (make-real x)
  ((get 'make 'real) x))

(install-real-package)

;;; Rationals

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (square-rat x)
    (make-rat (square (numer x)) (square (denom x))))
  (define (to-real x)
    (make-real (* 1.0 (/ (numer x) (denom x)))))
  (define (real-op op)
    (lambda (x) (op (to-real x))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational) 
       (lambda (x) (=zero? (numer x))))
  (put 'sine '(rational) (real-op sine))
  (put 'cosine '(rational) (real-op cosine))
  (put 'arc-tangent '(rational rational) 
       (lambda (x y)
         (arc-tangent (to-real x) (to-real y))))
  (put 'square '(rational) 
       (lambda (x) (tag (square-rat x))))
  (put 'square-root '(rational) (real-op square-root))
  (put 'raise '(rational)
       (lambda (x) (to-real x)))
  (put 'project '(rational)
       (lambda (x) (numer x)))
  (put 'make 'rational
       (lambda (n d) 
         (if (not (eq? 'integer (type-tag n))) (error "Numerator not integer" n))
         (if (not (eq? 'integer (type-tag d))) (error "Denominator not integer" d))
         (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(install-rational-package)

;;; Complex numbers

;; Rectangular complex numbers

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))
  (define (angle z)
    (arc-tangent (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (mul r (cosine a)) (mul r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'equ? '(rectangular rectangular)
       (lambda (x y)
         (and (equ? (real-part x) (real-part y))
              (equ? (imag-part x) (imag-part y)))))
  (put '=zero? '(rectangular)
       (lambda (x) (and (=zero? (real-part x)) (=zero? (imag-part x)))))
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Polar complex numbers

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (square-root (add (square x) (square y)))
          (arc-tangent y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'equ? '(polar polar)
       (lambda (x y)
         (and (equ? (magnitude x) (magnitude y))
              (equ? (angle x) (angle y)))))
  (put '=zero? '(polar)
       (lambda (x) (=zero? (magnitude x))))
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Container complex number package

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'equ? '(complex complex)
       (lambda (x y)
         (if (eq? (type-tag x) (type-tag y))
             ;; revert to "native" rectangular and polar implementations 
             ;; if numbers have the same type
             (equ? x y)
             ;; compare real and imaginary parts if numbers have different
             ;; types
             ((get 'equ? '(real-part)) (contents x) (contents y)))))
  (put '=zero? '(complex) =zero?)
  (put 'project '(complex) real-part)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Generic functions

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Constructors

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Installation

(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;;

(define (can-raise-to-type? value type)
  (if (get 'raise (list (type-tag value)))
      (let ((raised (raise value)))
        (if (eq? (type-tag raised) type)
            true
            (can-raise-to-type? raised type)))
      false))

(define (higher-type-value x y)
  (let ((type-x (type-tag x))
        (type-y (type-tag y)))
    (cond 
      ((eq? type-x type-y) x)
      ((can-raise-to-type? x type-y) y)
      ((can-raise-to-type? y type-x) x)
      (else (error "Can't raise either value to each other's types" x y)))))

(define (highest-type lst)
  (cond
    ((null? lst) (error "No element in list"))
    ((null? (cdr lst)) (type-tag (car lst)))
    (else (highest-type (cons (higher-type-value (car lst) (cadr lst)) (cddr lst))))))

(define (raise-till type value)
  (if (eq? type (type-tag value))
      value
      (raise-till type (raise value))))

(define (drop x)
  (if (get 'project (list (type-tag x)))
      (let ((projected (project x)))
        (if (equ? (raise projected) x)
            (drop projected)
            x))
      x))

(define (apply-generic op . args)
  (define (drop-if-number x) 
    (if (not (boolean? x))
        (drop x)
        x))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (drop-if-number (apply proc (map contents args)))
          (let* ((highest-type (highest-type args))
                 (raised-args (map (lambda (x) (raise-till highest-type x)) args)))
            (drop-if-number (apply apply-generic op raised-args)))))))