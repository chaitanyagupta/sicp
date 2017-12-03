#lang sicp

(define (make-monitored f)
  (let ((count 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls) count)
            ((eq? x 'reset-count) (set! count 0))
            (else (set! count (+ count 1))
                  (f x))))))
