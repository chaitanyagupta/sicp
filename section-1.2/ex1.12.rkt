#lang planet neil/sicp

;; i is row, j is column
;; i,j are 0 indexed
;; special conditions: if i < 0 or j < 0, return 0
;;                     if j > i, return 0
(define (pascal i j)
  (cond ((or (< i 0) (< j 0)) 0)
        ((> j i) 0)
        ((or (= j 0) (= j i)) 1)
        (else (+ (pascal (- i 1) (- j 1))
                 (pascal (- i 1) j)))))
