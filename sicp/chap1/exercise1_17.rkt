; Recurcive Multiplication
(define (double a) (+ a a))
(define (halve a) (/ a 2))
; assume that one only needs to take b into consideration as a potential even number
(define (* a b)
  (cond [(= b 0) 0]
        [(even? b) (* (double a) (halve b))]
        [else (+ a(* a (- b 1)))]))