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
    (if (and (not (null? stream))
             (> n 0))
        (begin (if (< n limit) (display #\ ))
               (display (stream-car stream))
               (iter (stream-cdr stream) (- n 1)))))
  (iter s limit)
  (display #\)))

(define (sign-change-detector new-value last-value)
  (cond ((and (< new-value 0) (> last-value 0)) -1)
        ((and (> new-value 0) (< last-value 0)) +1)
        (else 0)))

(define (make-zero-crossings input-stream last-value)
  (if (stream-null? input-stream)
      the-empty-stream
      (cons-stream
       (sign-change-detector (stream-car input-stream) last-value)
       (make-zero-crossings (stream-cdr input-stream)
                            (stream-car input-stream)))))

(define (list-stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list)
                   (list-stream (cdr list)))))

(define (smooth s)
  (stream-map (lambda (x y) (/ (+ x y) 2)) (stream-cdr s) s))

(define sense-data (list-stream '(1  2  1.5  1  0.5  -0.1  -2  -3  -2  -0.5  0.2  3  4)))
(define zero-crossings (make-zero-crossings (smooth sense-data) 0))

(define sense-data-2 (list-stream '(1  2  1.5  1  0.5  -0.1  -2  1  -2  -0.5  0.2  3  4)))
(define zero-crossings-2 (make-zero-crossings (smooth sense-data-2) 0))