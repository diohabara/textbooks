; Modified fixed-point
(define tolerance 0.00001)
(define (modified-fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (display "old value is: ")
    (display v1)
    (newline)
    (display "new value is: ")
    (display v2)
    (newline)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(define (average a b) (/ (+ a b) 2.0))

; without averave damping
(display "without averave damping")
(newline)
(modified-fixed-point (lambda (x) (/ (log 1000) (log x)))
                      2) ; 4.555532270803653

; with average damping
(newline)
(display "with averave damping")
(newline)
(modified-fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2) ; 4.555537551999825