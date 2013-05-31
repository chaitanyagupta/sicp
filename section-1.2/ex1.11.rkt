#lang planet neil/sicp

(define (rec-f n)
  (if (< n 3)
      n
      (+ (rec-f (- n 1))
         (* 2 (rec-f (- n 2)))
         (* 3 (rec-f (- n 3))))))

(define (iter-f n)
  (define (iter a b c n)
    (cond ((= n 0) c)
          ((= n 1) b)
          ((= n 2) a)
          (else (iter (+ a (* 2 b) (* 3 c)) 
                      a 
                      b 
                      (- n 1)))))
  (iter 2 1 0 n))
