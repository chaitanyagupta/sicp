#lang scheme

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define nil '())

(define (connect-vectors vectors)
  (if (or (null? vectors) (null? (cdr vectors)))
      nil
      (cons (make-segment (car vectors) (cadr vectors))
            (connect-vectors (cdr vectors)))))
      

;; a.  The painter that draws the outline of the designated frame.
(define outline-painter (segments->painter (connect-vectors (list (make-vect 0 0) 
                                                                  (make-vect 1 0) 
                                                                  (make-vect 1 1) 
                                                                  (make-vect 0 1) 
                                                                  (make-vect 0 0)))))

;; b.  The painter that draws an ``X'' by connecting opposite corners of the frame.
(define x-painter (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                                           (make-segment (make-vect 1 0) (make-vect 0 1)))))

;; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
(define diamond-painter (segments->painter (connect-vectors (list (make-vect 0.5 0)
                                                                  (make-vect 1 0.5)
                                                                  (make-vect 0.5 1)
                                                                  (make-vect 0 0.5)
                                                                  (make-vect 0.5 0)))))

;; d.  The wave painter.


