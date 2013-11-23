#lang planet neil/sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (make-record key value)
  (cons key value))

(define (key record)
  (car record))

(define (value record)
  (cdr record))

(define (lookup given-key set-of-records)
  (if (null? set-of-records) 
      false
      (let ((entry-key (key (entry set-of-records))))
        (cond ((= given-key entry-key) (entry set-of-records))
              ((< given-key entry-key) 
               (lookup given-key (left-branch set-of-records)))
              ((> given-key entry-key)
               (lookup given-key (right-branch set-of-records)))))))

