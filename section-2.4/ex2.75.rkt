#lang planet neil/sicp

(define (square x) (* x x))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (lambda (op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) 
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else 
           (error "Uknown op -- MAKE-FROM-MAG-ANG")))))

(define (apply-generic op arg) (arg op))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; The constant pi
(define pi 3.14159265358979323846264338328)
  
;; Pi divided by two, pi/2
(define pi/2 1.57079632679489661923132169164)
  
;; Pi divided by four, pi/4
(define pi/4 0.78539816339744830966156608458)

