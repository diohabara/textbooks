; Finding Golden Ratio
(define (golden-ratio x)
  (fixed-point (lambda (x) (average x (+ 1 (/ 1 x))))
             1.0))
; finding fixed points
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average a b)
  (/ (+ a b) 2.0))