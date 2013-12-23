#lang planet neil/sicp

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 y1 x2 y2 trials)
  (define (test)
    (let ((point-x (random-in-range x1 x2))
          (point-y (random-in-range y1 y2)))
      (P point-x point-y)))
  (let* ((area (* (abs (- x2 x1)) (abs (- y2 y1))))
         (fraction (monte-carlo trials test)))
    (* area fraction)))
    
        
                 