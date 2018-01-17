#lang sicp

;;; copy the following forms in the REPL for ambiguous evaluator

(define (not x)
  (if x false true))

(define (require p)
  (if (not p) (amb)))

(define (map f items)
  (if (null? items)
      '()
      (cons (f (car items)) (map f (cdr items)))))

(define (for-each f items)
  (if (null? items)
      'ok
      (begin (f (car items))
             (for-each f (cdr items)))))

(define (member item items)
  (cond ((null? items) false)
        ((eq? item (car items)) items)
        (else (member item (cdr items)))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define *connections* (list (list 'moore '() '())
                            (list 'downing '() '())
                            (list 'hall '() '())
                            (list 'hood '() '())
                            (list 'parker '() '())))

(define (connection-father connection)
  (car connection))

(define (connection-daughter connection)
  (car (cdr connection)))

(define (connection-yatch connection)
  (car (cdr (cdr connection))))

(define (set-connection-daughter! connection daughter)
  (set-cdr! connection (list daughter (connection-yatch connection)))
  'ok)

(define (set-connection-yatch! connection yatch)
  (set-cdr! (cdr connection) (list yatch))
  'ok)

(define (find-connection father)
  (define (iter connections)
    (cond ((null? connections) false)
          ((eq? (car (car connections)) father) (car connections))
          (else (iter (cdr connections)))))
  (iter *connections*))

(define (find-connection-for-daughter daughter)
  (define (iter connections)
    (cond ((null? connections) false)
          ((eq? (car (cdr (car connections))) daughter) (car connections))
          (else (iter (cdr connections)))))
  (iter *connections*))

(define (daughter father)
  (connection-daughter (find-connection father)))

(define (yatch father)
  (connection-yatch (find-connection father)))

(define (set-daughter! father daughter)
  (set-connection-daughter! (find-connection father) daughter))

(define (set-yatch! father yatch)
  (set-connection-yatch! (find-connection father) yatch))

(define (a-father)
  (amb 'moore 'downing 'hall 'hood 'parker))

(define (a-daughter)
  (amb 'mary-ann 'gabrielle 'lorna 'rosalind 'melissa))

(define (a-yatch)
  (a-daughter))

(define (make-connections)
  (set-daughter! 'moore 'mary-ann)
  (set-yatch! 'moore 'lorna)

  (set-daughter! 'downing (a-daughter))
  (set-yatch! 'downing 'melissa)

  (set-daughter! 'hall (a-daughter))
  (set-yatch! 'hall 'rosalind)

  (set-daughter! 'hood (a-daughter))
  (set-yatch! 'hood 'gabrielle)
  (require (eq? (daughter 'hood) (yatch 'downing)))

  (set-daughter! 'parker (a-daughter))
  (set-yatch! 'parker (a-yatch))

  (for-each (lambda (connection)
              (require (not (eq? (connection-daughter connection)
                                 (connection-yatch connection)))))
            *connections*)
  (require (distinct? (map connection-daughter *connections*)))
  (require (distinct? (map connection-yatch *connections*)))

  (require (eq? (connection-yatch (find-connection-for-daughter 'gabrielle))
                (daughter 'parker)))

  *connections*)
