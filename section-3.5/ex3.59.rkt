#lang sicp

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
    (if (not (null? (stream-cdr stream)))
        (if (> n 0)
            (begin (display (stream-car stream))
                   (if (> n 1) (display #\ ))
                   (iter (stream-cdr stream) (- n 1))))))
  (iter s limit)
  (display #\)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define (integrate-series stream)
  (define (recurse stream n)
    (cons-stream (/ (stream-car stream) n)
                 (recurse (stream-cdr stream) (+ n 1))))
  (recurse stream 1))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (stream-map - sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
