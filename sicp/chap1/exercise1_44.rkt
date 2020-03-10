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
; smoothing function
(define (smooth f)
  (define dx 0.00001)
  (define (average a b c)
    (/ (+ a b c) 3.0))
  (lambda (x)
    (average (f (- x dx)) (f x) (f (+ x dx)))))