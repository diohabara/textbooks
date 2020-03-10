;; 2.75
; implement the constructor make-from-mag-ang in message-passing style
(define (make-from-mag-ang  r a)
  (define (dispatch op)
    (cond [(eq? op 'real-part) (* r (cos a))]
          [(eq? op 'imag-part) (* r (sin a))]
          [(eq? op 'magnitude) r]
          [(eq? op 'angle) a]
          [else (error "Unknown op: MAKE-FROM-MAG-ANG" op)]))
  dispatch)

;; 2.76
; <generic operations with explicit dispatch>
; bad: when new types are added, all procedures need to be cahnged
; <data-directed>
; bad: when new types are added, have to make package for new type
; <message-passing
; bad: when new operations are added, new operation must be added
; for new types, message-passing
; for new operations, data-directed