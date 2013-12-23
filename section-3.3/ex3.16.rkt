#lang planet neil/sicp

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))


;; > (count-pairs '(a b c))
;; 3
;; > (count-pairs (cons (cons 'b nil) (cons 'c nil)))
;; 3
;; > (let* ((c-pair (cons 'c nil))
;;          (b-pair (cons 'b c-pair)))
;;     (count-pairs (cons b-pair c-pair)))
;; 4
;; > (let* ((c-pair (cons 'c nil))
;;          (b-pair (cons c-pair c-pair)))
;;     (count-pairs (cons b-pair b-pair)))
;; 7
;; > (let* ((c-pair (cons 'c nil))
;;          (b-pair (cons 'b c-pair))
;;          (a-pair (cons 'a b-pair)))
;;     (set-cdr! c-pair a-pair)
;;     (count-pairs a-pair))
