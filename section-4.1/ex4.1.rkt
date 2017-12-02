#lang planet neil/sicp

;; left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operands exp) env)))
        (cons first (list-of-values (rest-operands exp) env)))))


;; right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values (rest-operands exp) env)))
        (cons (eval (first-operands exp) env) rest))))