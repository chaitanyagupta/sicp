#lang sicp

;;; copy the following forms in the REPL for ambiguous evaluator

(define (not x)
  (if x false true))

(define (require p)
  (if (not p) (amb)))

(define (make-position row column)
  (cons row column))

(define (row position)
  (car position))

(define (column position)
  (cdr position))

(define (adjoin-position position positions)
  (cons position positions))

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define board-size 8)

(define (a-row)
  (an-integer-between 1 board-size))

(define (a-position-in-column k)
  (make-position (a-row) k))

(define (safe? positions)
  (let ((target (car positions)))
    (define (iter rest)
      (if (null? rest)
          true
          (let ((against (car rest)))
            (cond ((= (row target) (row against)) false)
                  ((= (abs (- (row target) (row against)))
                      (abs (- (column target) (column against)))) false)
                  (else (iter (cdr rest)))))))
    (iter (cdr positions))))

(define (solve k)
  (if (= k 1)
      (list (a-position-in-column k))
      (let* ((previous (solve (- k 1)))
             (position (a-position-in-column k))
             (current (adjoin-position position previous)))
        (require (safe? current))
        current)))
