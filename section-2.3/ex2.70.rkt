#lang planet neil/sicp

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-aux symbol tree)
    (if (leaf? tree)
        '()
        (if (memq symbol (symbols (left-branch tree)))
            (cons 0 (encode-symbol symbol (left-branch tree)))
            (cons 1 (encode-symbol symbol (right-branch tree))))))
  (if (memq symbol (symbols tree))
      (encode-symbol-aux symbol tree)
      (error "symbol not in tree" symbol)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (cond ((null? set)
         (error "Didn't expect an empty set"))
        ((null? (cdr set))
         (car set))
        (else 
         (successive-merge (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set))))))

(define rock-pairs '((A 2)
                     (NA 16)
                     (BOOM 1)
                     (SHA 3)
                     (GET 2)
                     (YIP 9)
                     (JOB 2)
                     (WAH 1)))

(define rock-tree (generate-huffman-tree rock-pairs))

;; > (encode '(get a job
;;                sha na na na na na na na na
;;                get a job
;;                sha na na na na na na na na
;;                wah yip yip yip yip yip yip yip yip yip
;;                sha boom) 
;;          rock-tree)
;; {1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1}
;; 84 bits required for encoding

;; If fixed-length encoding was used, then we would require 3 bits per word (8 words in total).
;; Since there are 36 words in the song, total number of bits required would be 3 * 36 = 108