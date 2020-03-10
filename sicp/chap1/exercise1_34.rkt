(define (f g) (g 2))
(define (square x) (* x x))
; Examples
(f square) ; 4
(f (lambda (z) (* z (+ z 1)))) ; 6

; What happens?
; Ans: Nothing
; Reason: Return no value
; (f f)
; (f 2)
; (2 2)
(f f)