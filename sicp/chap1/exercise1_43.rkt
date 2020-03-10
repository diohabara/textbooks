(define (square x) (* x x))
;; compose two functions together
(define (compose f g)
  (lambda (x) (f (g x))))
; repeat mapping n times
(define (repeated f n)
  (define (repeat-iter g cnt)
    (if (>= cnt n)
        g
        (repeat-iter (compose g f) (+ cnt 1))))
  (repeat-iter f 1))

;result
((repeated square 2) 5) ; 625
((repeated square 3) 2) ; 256 = 2^8