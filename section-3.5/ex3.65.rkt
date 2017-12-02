#lang planet neil/sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s limit)
  (display #\()
  (define (iter stream n)
    (if (and (not (null? stream))
             (> n 0))
        (begin (if (< n limit) (display #\ ))
               (display (stream-car stream))
               (iter (stream-cdr stream) (- n 1)))))
  (iter s limit)
  (display #\)))

(define (square x) (* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-limit stream tolerance)
  (define (iter s previous)
    (let ((current (stream-car s)))
      (if (< (abs (- current previous)) tolerance)
          current
          (iter (stream-cdr s) current))))
  (iter (stream-cdr stream) (stream-car stream)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (partial-sums stream)
  (define sums (add-streams stream (cons-stream 0 sums)))
  sums)

(define ln2-stream (partial-sums
                    (stream-map (lambda (x) 
                                  (/ 1 (if (even? x) (- x) x))) 
                                integers)))

(define expected 0.6931471805599453)
(define expected-tol 1e-3)
(define (iterations-to-converge s)
  (if (< (abs (- (stream-car s) expected)) expected-tol)
      0
      (inc (iterations-to-converge (stream-cdr s)))))
