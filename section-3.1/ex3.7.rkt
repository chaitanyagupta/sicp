#lang sicp

(define (make-password-checker account-password)
  (lambda (password)
    (eq? password account-password)))

(define (incorrect-password x)
  "Incorrect Password")

(define (make-account balance account-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((password-checker (make-password-checker account-password)))
    (define (dispatch password m)
      (cond ((not (password-checker password)) incorrect-password)
            ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (make-joint original original-password account-password)
  (let ((password-checker (make-password-checker account-password)))
    (define (dispatch password m)
      (if (not (password-checker password))
          incorrect-password
          (original original-password m)))
    dispatch))
