#lang planet neil/sicp

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; In a-wires, b-wires and s-wires, MSB is first
(define (ripple-carry-adder a-wires b-wires s-wires c)
  (let ((c-in (make-wire)))
    (full-adder (car a-wires) (car b-wires) c-in (car s-wires) c)
    (if (cdr a-wires)
        (ripple-carry-adder (cdr a-wires) (cdr b-wires) (cdr s-wires) c-in)
        (set-signal! c-in 0))))