#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define tree1 (make-tree 7 
                         (make-tree 3 
                                    (make-tree 1 nil nil)
                                    (make-tree 5 nil nil))
                         (make-tree 9
                                    nil
                                    (make-tree 11 nil nil))))

(define tree2 (make-tree 3
                         (make-tree 1 nil nil)
                         (make-tree 7
                                    (make-tree 5 nil nil)
                                    (make-tree 9 
                                               nil
                                               (make-tree 11 nil nil)))))

(define tree3 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 nil nil)
                                    nil)
                         (make-tree 9
                                    (make-tree 7 nil nil)
                                    (make-tree 11 nil nil))))

(define simple-tree-1 (make-tree 3 nil nil))
(define simple-tree-2 (make-tree 5 (make-tree 3 nil nil) (make-tree 7 nil nil)))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (union-set-list set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else 
     (let ((x1 (car set1)) (x2 (car set2)))
       (cond 
         ((= x1 x2) (cons x1 (union-set-list (cdr set1) (cdr set2))))
         ((< x1 x2) (cons x1 (union-set-list (cdr set1) set2)))
         (else (cons x2 (union-set-list set1 (cdr set2)))))))))       

(define (intersection-set-list set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-list (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set-list (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-list set1 (cdr set2)))))))

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (union-set-list list1 list2))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (intersection-set-list list1 list2))))

