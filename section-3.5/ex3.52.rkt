#lang planet neil/sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-line x)
  (display x)
  (newline))

(define (show x)
  (display-line x)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

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

(define (display-stream s)
  (stream-for-each display-line s))

(define sum 0)
; sum => 0

(define (accum x)
  (set! sum (+ x sum))
  sum)
; sum => 0

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum => 1

(define y (stream-filter even? seq))
; sum => 6
; enm => (1 2 3 4  5  6  7  8  9  10 11 12 13 14  15  16  17  18  19  20 )
; seq => (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
; y   => (    6 10       28 36       66 78        120 136         190 210)

(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
; sum => 10

(stream-ref y 7)
; sum => 136

(display-stream z)
; sum => 210
; enm => (1 2 3 4  5  6  7  8  9  10 11 12 13 14  15  16  17  18  19  20 )
; seq => (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153 171 190 210)
; z   => (      10 15          45 55          105 120             190 210) 