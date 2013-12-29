#lang planet neil/sicp

;;; Deque node

(define (make-deque-node item) (cons item (cons '() '())))
(define (node-item node) (car node))
(define (previous-node node) (cadr node))
(define (next-node node) (cddr node))
(define (set-previous-node! node previous-node)
  (set-car! (cdr node) previous-node))
(define (set-next-node! node next-node)
  (set-cdr! (cdr node) next-node))

;;; Deque

(define (make-deque) (cons '() '()))

(define (front-node deque) (car deque))
(define (rear-node deque) (cdr deque))
(define (set-front-node! deque item) (set-car! deque item))
(define (set-rear-node! deque item) (set-cdr! deque item))

(define (empty-deque? deque) (null? (front-node deque)))

(define (ensure-non-empty-deque deque)
  (if (empty-deque? deque)
      (error "Did not expect an empty deque" deque)))

(define (front-deque deque)
  (ensure-non-empty-deque deque)
  (node-item (front-node deque)))

(define (rear-deque deque)
  (ensure-non-empty-deque deque)
  (node-item (rear-node deque)))

(define (front-insert-deque! deque item)
  (let ((new-node (make-deque-node item)))
    (cond ((empty-deque? deque)
           (set-front-node! deque new-node)
           (set-rear-node! deque new-node))
          (else
           (set-next-node! new-node (front-node deque))
           (set-previous-node! (front-node deque) new-node)
           (set-front-node! deque new-node)))
    deque))

(define (rear-insert-deque! deque item)
  (let ((new-node (make-deque-node item)))
    (cond ((empty-deque? deque)
           (set-front-node! deque new-node)
           (set-rear-node! deque new-node))
          (else
           (set-previous-node! new-node (rear-node deque))
           (set-next-node! (rear-node deque) new-node)
           (set-rear-node! deque new-node)))
    deque))

(define (front-delete-deque! deque)
  (ensure-non-empty-deque deque)
  (cond ((eq? (front-node deque) (rear-node deque))
         (set-front-node! deque '())
         (set-rear-node! deque '()))
        (else
         (set-previous-node! (next-node (front-node deque)) '())
         (set-front-node! deque (next-node (front-node deque)))))
  deque)

(define (rear-delete-deque! deque)
  (ensure-non-empty-deque deque)
  (cond ((eq? (front-node deque) (rear-node deque))
         (set-front-node! deque '())
         (set-rear-node! deque '()))
        (else
         (set-next-node! (previous-node (rear-node deque)) '())
         (set-rear-node! deque (previous-node (rear-node deque)))))
  deque)

(define (print-deque deque)
  (display "[")
  (define (print-items node)
    (if (not (null? node))
        (begin (display (node-item node))
               (if (not (eq? node (rear-node deque)))
                   (display " "))
               (print-items (next-node node)))))
  (print-items (front-node deque))
  (display "]"))
