#lang sicp

(define origin-frame frame-origin)
(define edge1-frame frame-edge1)
(define edge2-frame frame-edge2)

;;; 

(define xcor-vect vector-xcor)
(define ycor-vect vector-ycor)
(define add-vect vector-add)
(define sub-vect vector-sub)
(define scale-vect vector-scale)

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ((m (frame-coord-map frame))
           (new-origin (m origin)))
      (painter (make-frame new-origin
                           (sub-vect (m corner1) new-origin)
                           (sub-vect (m corner2) new-origin))))))

(define (below1 painter1 painter2)
  (let ((paint-below (transform-painter painter1
                                        (make-vect 0 0)
                                        (make-vect 1 0)
                                        (make-vect 0 0.5)))
        (paint-above (transform-painter painter2
                                        (make-vect 0 0.5)
                                        (make-vect 1 0.5)
                                        (make-vect 0 1))))
    (lambda (frame)
      (paint-below frame)
      (paint-above frame))))

(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))
